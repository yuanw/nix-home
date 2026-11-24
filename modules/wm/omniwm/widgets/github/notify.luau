-- What one published GitHub thread looks like on the bar and where its row
-- takes you. Pure (no omniwm/ui), so tests/github-widget.nix can run it.
--
-- modules/wm/omniwm/github-inbox.sh publishes the threads with the short keys
-- it uses here. A notification subject carries no html_url, so the page is
-- derived from the subject's api url and the repository's html_url; "p", the
-- pull request's own page, wins when the filter fetched it.

local KINDS = {
    Issue = { icon = "number", color = "#a6da95" }, -- GREEN, modules/wm/sketchybar/colors.sh
    Discussion = { icon = "text.bubble", color = "#cad3f5" }, -- WHITE
    PullRequest = { icon = "arrow.triangle.pull", color = "#c6a0f6" }, -- MAGENTA
    Commit = { icon = "arrow.triangle.branch", color = "#cad3f5" }, -- WHITE
}

local UNKNOWN = { icon = "circle", color = "#cad3f5" }

local INBOX = "https://www.github.com/notifications"

local function text(value)
    return if type(value) == "string" then value else ""
end

-- The plugin grepped the title for these to paint the bell red.
local function important(title)
    return string.find(string.lower(title), "deprecat") ~= nil
        or string.find(string.lower(title), "break") ~= nil
        or string.find(string.lower(title), "broke") ~= nil
end

local function page(item)
    if item.k == "Discussion" then
        return INBOX
    end
    if text(item.p) ~= "" then
        return item.p
    end
    local repo = text(item.h)
    if repo == "" then
        return INBOX
    end
    local subject = text(item.s)
    local number = string.match(subject, "/pulls/(%d+)$")
    if number then
        return repo .. "/pull/" .. number
    end
    number = string.match(subject, "/issues/(%d+)$")
    if number then
        return repo .. "/issues/" .. number
    end
    local commit = string.match(subject, "/commits/(%w+)$")
    if commit then
        return repo .. "/commit/" .. commit
    end
    return repo
end

-- payload: the JSON github-inbox.sh published, already decoded.
local function rows(payload)
    local items = {}
    local published = type(payload.i) == "table" and payload.i or {}
    for _, item in published do
        if type(item) == "table" then
            local kind = KINDS[item.k] or UNKNOWN
            local title = text(item.t)
            items[#items + 1] = {
                repo = text(item.r),
                title = title,
                icon = kind.icon,
                color = kind.color,
                url = page(item),
                important = important(title),
            }
        end
    end
    return items
end

return { rows = rows, page = page, important = important, INBOX = INBOX }
