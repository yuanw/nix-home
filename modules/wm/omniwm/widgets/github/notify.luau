-- What one GitHub notification thread looks like on the bar and where its row
-- takes you. Pure (no omniwm/ui), so tests/github-widget.nix can run it.
--
-- A notification subject carries no html_url, so the page is derived from the
-- subject's api url and the repository's html_url; init.luau passes the pull
-- request it fetched for the state check, whose html_url wins when present.

local KINDS = {
    Issue = { icon = "number", color = "#a6da95" }, -- GREEN, modules/wm/sketchybar/colors.sh
    Discussion = { icon = "text.bubble", color = "#cad3f5" }, -- WHITE
    PullRequest = { icon = "arrow.triangle.pull", color = "#c6a0f6" }, -- MAGENTA
    Commit = { icon = "arrow.triangle.branch", color = "#cad3f5" }, -- WHITE
}

local UNKNOWN = { icon = "circle", color = "#cad3f5" }

local INBOX = "https://www.github.com/notifications"

-- The plugin grepped the title for these to paint the bell red.
local function important(title)
    return string.find(string.lower(title), "deprecat") ~= nil
        or string.find(string.lower(title), "break") ~= nil
        or string.find(string.lower(title), "broke") ~= nil
end

local function page(thread, pull)
    local subject = thread.subject or {}
    local repository = thread.repository or {}
    if subject.type == "Discussion" then
        return INBOX
    end
    if type(pull) == "table" and type(pull.html_url) == "string" and pull.html_url ~= "" then
        return pull.html_url
    end
    local repo = repository.html_url
    if type(repo) ~= "string" or repo == "" then
        return INBOX
    end
    local subjectUrl = type(subject.url) == "string" and subject.url or ""
    local number = string.match(subjectUrl, "/pulls/(%d+)$")
    if number then
        return repo .. "/pull/" .. number
    end
    number = string.match(subjectUrl, "/issues/(%d+)$")
    if number then
        return repo .. "/issues/" .. number
    end
    local commit = string.match(subjectUrl, "/commits/(%w+)$")
    if commit then
        return repo .. "/commit/" .. commit
    end
    return repo
end

-- thread: one object from GET /notifications. pullState: the state the pull
-- request fetch returned, or nil when it was not looked up (a team mention,
-- or the state-check budget was spent). Keeping a thread whose state is
-- unknown is deliberate: the python filter dismissed it, which let a failed
-- gh call mark real notifications read.
local function decide(thread, pullState)
    if thread.reason == "team_mention" and type(thread.subject) == "table"
        and thread.subject.type == "PullRequest"
    then
        return "dismiss"
    end
    if pullState == nil then
        return "keep"
    end
    return if pullState == "open" then "keep" else "dismiss"
end

-- thread: one object from GET /notifications. pull: the PR GET /repos/.../pulls/n
-- returned, when the thread is an open pull request.
local function row(thread, pull)
    local subject = thread.subject or {}
    local repository = thread.repository or {}
    local title = type(subject.title) == "string" and subject.title or ""
    local kind = KINDS[subject.type] or UNKNOWN
    return {
        repo = type(repository.name) == "string" and repository.name or "",
        title = title,
        icon = kind.icon,
        color = kind.color,
        url = page(thread, pull),
        important = important(title),
    }
end

return { row = row, decide = decide, important = important, page = page, INBOX = INBOX }
