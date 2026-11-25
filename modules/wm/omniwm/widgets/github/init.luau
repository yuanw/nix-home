-- The GitHub inbox button: the unread count on the bar, the threads in a
-- popout, and a row that opens its page. The port of
-- modules/wm/sketchybar/plugins/github.sh.
--
-- No token and no network permission here. modules/wm/omniwm/github-inbox.sh
-- runs gh and github-notifications-filter.py on a launchd timer, the way that
-- plugin ran them itself, and publishes the result with `omniwmctl shell set`;
-- the host gives every widget what it published as ctx.data.vars[KEY], and a
-- change to it is what repaints the button.

local look = require("../lib/look")
local notify = require("@self/notify") -- @self/, not ./: from init.luau ./ is one folder up

local NAME = "github" -- this folder's name; "shell toggle" closes this popout
local KEY = "github-inbox" -- the key github-inbox.sh publishes
local MAX_ROWS = 12
local COUNT_COLOR = "#8aadf4" -- BLUE, modules/wm/sketchybar/colors.sh
local ALERT_COLOR = "#ed8796" -- RED, the colour a breaking title painted the bell

-- Decode once per published value and remember the count, so the renders that
-- follow a repaint do not decode it again or bounce the bell twice. View hooks
-- must not start work, so this only reads what the host handed over.
local function inbox(ctx)
    local state = ctx.state
    local raw = ctx.data.vars[KEY]
    if state.items ~= nil and raw == state.raw then
        return state
    end
    state.raw = raw
    state.items = {}
    state.count = 0
    state.error = if raw == nil then "Waiting for the first inbox update" else "The published inbox is not JSON"
    local payload = if type(raw) == "string" then json.decode(raw) else nil
    if type(payload) ~= "table" then
        return state
    end
    state.items = notify.rows(payload)
    state.count = tonumber(payload.n) or #state.items
    state.error = if type(payload.e) == "string" and payload.e ~= "" then payload.e else nil
    state.important = false
    for _, item in state.items do
        if item.important then
            state.important = true
        end
    end
    if state.count > (state.watched or 0) then
        -- a new pulse key is the bell bounce the plugin animated
        state.pulse = (state.pulse or 0) + 1
    end
    state.watched = state.count
    return state
end

function render(ctx)
    local state = inbox(ctx)
    local scale = look.scale(ctx)
    local glyph = look.glyph{
        name = if state.count > 0 then "bell.badge.fill" else "bell",
        size = 14 * scale,
        color = if state.important then ALERT_COLOR elseif state.count > 0 then COUNT_COLOR else nil,
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
        look.text{ text = tostring(state.count), size = 13 * scale, reserve = "88" },
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
    local state = inbox(ctx)
    local width = look.panelWidth(NAME)
    local rows = { w = width }
    rows[#rows + 1] = ui.row{
        w = width - 28,
        gap = 6,
        look.text{ text = "GitHub", style = "bold", grow = 1 },
    }
    if state.error then
        rows[#rows + 1] = look.text{
            text = state.error,
            size = 11,
            color = ALERT_COLOR,
            maxWidth = width - 28,
        }
    end
    if #state.items == 0 then
        rows[#rows + 1] = look.text{
            text = "No new notifications",
            size = 12,
            color = "secondaryForeground",
        }
    end
    for index = 1, math.min(#state.items, MAX_ROWS) do
        rows[#rows + 1] = threadRow(state.items[index], width - 28)
    end
    if state.count > #state.items then
        rows[#rows + 1] = look.text{
            text = string.format("%d more in your inbox", state.count - #state.items),
            size = 11,
            color = "secondaryForeground",
        }
    end
    return look.panel(ctx, rows)
end
