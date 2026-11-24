-- A GitHub inbox on the workspace bar: the unread count on the button, the
-- threads in a popout, and a row that opens its page. The port of
-- modules/wm/sketchybar/plugins/github.sh and github-notifications-filter.py.
--
-- The Luau shell starts no programs, so where that pair shelled out to gh and
-- python this polls api.github.com with omniwm.http. settings.toml beside this
-- file declares that host and the three options: the token, the poll interval,
-- and whether pull requests that are already closed get marked read.

local look = require("../lib/look")
local notify = require("@self/notify") -- @self/, not ./: from init.luau ./ is one folder up

local NAME = "github" -- this folder's name; "shell toggle" closes this popout
local API = "https://api.github.com"
local PER_PAGE = 100
local MAX_THREADS = 300
local MAX_LOOKUPS = 30 -- pull request state checks per poll, the shared budget is six requests
local MAX_ROWS = 12
local COUNT_COLOR = "#8aadf4" -- BLUE, modules/wm/sketchybar/colors.sh
local ALERT_COLOR = "#ed8796" -- RED, the colour a breaking title painted the bell

local poll

local function settings(ctx)
    return ctx.data.settings or {}
end

local function headers(token)
    return {
        ["Authorization"] = "Bearer " .. token,
        Accept = "application/vnd.github+json",
        ["X-GitHub-Api-Version"] = "2022-11-28",
    }
end

-- JSON GETs, with an admitted failure reported to the same callback so every
-- caller handles one shape.
local function get(token, url, maxBytes, callback)
    local _, rejected = omniwm.http({
        url = url,
        headers = headers(token),
        response = "json",
        maxBytes = maxBytes,
        timeout = 30,
    }, callback)
    if rejected then
        callback({ ok = false, status = 0, headers = {}, error = rejected })
    end
end

local function dismiss(ctx, token, thread)
    if not settings(ctx).dismissClosed or type(thread.url) ~= "string" then
        return
    end
    local _, rejected = omniwm.http({
        url = thread.url,
        method = "PATCH",
        headers = headers(token),
        body = '{"read":true}',
        timeout = 30,
    }, function() end)
    if rejected then
        omniwm.log("github: could not mark a thread read: " .. tostring(rejected))
    end
end

local function fetchInbox(token, page, threads, done)
    get(token, API .. "/notifications?per_page=" .. PER_PAGE .. "&page=" .. page, 1048576, function(result)
        local batch = result.ok and type(result.json) == "table" and result.json or nil
        if not batch then
            done(nil, result.error or ("GitHub returned " .. tostring(result.status)))
            return
        end
        for index = 1, #batch do
            threads[#threads + 1] = batch[index]
        end
        if #batch == PER_PAGE and #threads < MAX_THREADS then
            fetchInbox(token, page + 1, threads, done)
        else
            done(threads, nil)
        end
    end)
end

-- Drop the threads the python filter dropped -- a team mention on a pull
-- request, and a pull request that is no longer open -- after marking them
-- read, and keep the rest as rows. One state check runs at a time, so the six
-- outstanding requests a widget may have stay free for the inbox itself.
local function liveRows(ctx, token, threads, done)
    local items = {}
    local index = 0
    local lookups = 0
    local function settle(thread, pull)
        if notify.decide(thread, pull and pull.state) == "dismiss" then
            dismiss(ctx, token, thread)
        else
            items[#items + 1] = notify.row(thread, pull)
        end
    end
    local step
    step = function()
        while index < #threads do
            index += 1
            local thread = threads[index]
            local subject = type(thread.subject) == "table" and thread.subject or {}
            local stateCheck = subject.type == "PullRequest"
                and thread.reason ~= "team_mention"
                and lookups < MAX_LOOKUPS
                and type(subject.url) == "string"
            if stateCheck then
                lookups += 1
                get(token, subject.url, 262144, function(result)
                    local pull = result.ok and type(result.json) == "table" and result.json or nil
                    settle(thread, pull)
                    step()
                end)
                return
            end
            settle(thread, nil)
        end
        done(items)
    end
    step()
end

local function schedule(ctx)
    local state = ctx.state
    local seconds = math.clamp(tonumber(settings(ctx).interval) or 120, 30, 3600)
    local timer, rejected = omniwm.after(seconds, function()
        state.timer = nil
        poll(ctx)
    end)
    state.timer = timer
    if not timer then
        state.error = "Polling paused: " .. tostring(rejected)
    end
end

poll = function(ctx)
    local state = ctx.state
    -- arm the next tick first: a poll whose callback never arrives must not be
    -- the last one this widget runs
    schedule(ctx)
    if state.busy then
        return
    end
    local token = settings(ctx).token
    if type(token) ~= "string" or token == "" then
        state.items = nil
        state.error = "Set a GitHub token in this widget's options"
        return
    end
    state.busy = true
    fetchInbox(token, 1, {}, function(threads, failure)
        if not threads then
            state.busy = false
            state.error = failure
            return
        end
        liveRows(ctx, token, threads, function(items)
            local before = state.items and #state.items or 0
            state.items = items
            state.important = false
            for _, item in items do
                if item.important then
                    state.important = true
                end
            end
            state.error = nil
            state.busy = false
            if #items > before then
                -- a new pulse key is the bell bounce the plugin animated
                state.pulse = (state.pulse or 0) + 1
            end
        end)
    end)
end

-- The popout's button: poll now instead of waiting for the timer, which the
-- next poll replaces anyway.
local function refresh(ctx)
    local state = ctx.state
    if state.busy then
        return
    end
    if state.timer then
        state.timer:cancel()
        state.timer = nil
    end
    poll(ctx)
end

function trigger(ctx, event)
    if event.source == "host" then
        poll(ctx)
    end
end

function render(ctx)
    local state = ctx.state
    local scale = look.scale(ctx)
    local count = state.items and #state.items or 0
    local glyph = look.glyph{
        name = if count > 0 then "bell.badge.fill" else "bell",
        size = 14 * scale,
        color = if state.important then ALERT_COLOR elseif count > 0 then COUNT_COLOR else nil,
    }
    if state.pulse then
        glyph.pulse = {
            key = NAME .. "-" .. tostring(state.pulse),
            duration = 0.4,
            rotate = { 0, -12, 0 },
        }
    end
    return look.bar(ctx, {
        onClick = ui.popout,
        help = "GitHub notifications",
        glyph,
        look.text{ text = tostring(count), size = 13 * scale, reserve = "88" },
    })
end

local function threadRow(item, width)
    return ui.row{
        w = width,
        gap = 8,
        pad = 6,
        radius = 8,
        hover = look.hover,
        cursor = "pointer",
        onClick = function()
            omniwm.open(item.url)
            omniwm.run("shell toggle " .. NAME)
        end,
        look.glyph{ name = item.icon, size = 13, color = item.color },
        look.text{ text = item.repo, size = 11, color = "secondaryForeground", maxWidth = 130 },
        look.text{ text = item.title, size = 12, grow = 1, maxWidth = width - 200 },
    }
end

function popout(ctx)
    local state = ctx.state
    local width = look.panelWidth(NAME)
    local items = state.items or {}
    local reload = if state.busy then nil else function()
        refresh(ctx)
    end
    local rows = { w = width }
    rows[#rows + 1] = ui.row{
        w = width - 28,
        gap = 6,
        look.text{ text = "GitHub", style = "bold", grow = 1 },
        look.button("arrow.clockwise", reload),
    }
    if state.error then
        rows[#rows + 1] = look.text{
            text = state.error,
            size = 11,
            color = ALERT_COLOR,
            maxWidth = width - 28,
        }
    end
    if #items == 0 and not state.busy then
        rows[#rows + 1] = look.text{
            text = "No new notifications",
            size = 12,
            color = "secondaryForeground",
        }
    end
    for index = 1, math.min(#items, MAX_ROWS) do
        rows[#rows + 1] = threadRow(items[index], width - 28)
    end
    if #items > MAX_ROWS then
        rows[#rows + 1] = look.text{
            text = string.format("%d more in your inbox", #items - MAX_ROWS),
            size = 11,
            color = "secondaryForeground",
        }
    end
    return look.panel(ctx, rows)
end
