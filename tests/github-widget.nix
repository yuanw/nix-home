{ pkgs }:

# The GitHub bar widget (modules/wm/omniwm/widgets/github) is Luau run by
# OmniWM, which this test does not have. Two checks instead: notify.luau holds
# the row/filter decisions and is plain Luau, and init.luau is concatenated
# with the fakes below so the polling flow -- pagination, the pull request
# state checks, the dismissals, the bar and popout trees -- actually runs.
# look.luau below is a stand-in for the style's lib/look.luau; the real one only
# turns these props into ui elements.
pkgs.runCommand "github-widget-tests"
  {
    nativeBuildInputs = [ pkgs.luau ];
    meta.description = "Regression tests for the OmniWM GitHub widget";
  }
  ''
    set -euo pipefail
    mkdir -p widgets/github
    cp ${./../modules/wm/omniwm/widgets/github/notify.luau} widgets/github/notify.luau

    # The luau CLI refuses a require that leaves the script's own folder and
    # knows nothing of @self/, so the copy under test finds its two modules
    # beside it. Those strings are the only edit this test makes to the widget.
    sed -e 's|require("../lib/look")|require("./look")|' \
        -e 's|require("@self/notify")|require("./notify")|' \
        ${./../modules/wm/omniwm/widgets/github/init.luau} > widgets/github/init.luau

    cat > widgets/github/look.luau <<'LUAU'
    local look = { hover = "foreground/0.08" }
    function look.scale(ctx)
        return (ctx.data.bar.style.height or 32) / 32
    end
    function look.panelWidth(name)
        return 320
    end
    function look.glyph(props)
        props.kind = "symbol"
        return props
    end
    function look.text(props)
        props.kind = "text"
        return props
    end
    function look.bar(ctx, props)
        props.kind = "bar"
        return props
    end
    function look.panel(ctx, props)
        props.kind = "panel"
        return props
    end
    function look.button(name, action)
        return { kind = "button", name = name, action = action }
    end
    return look
    LUAU

    cat > header.luau <<'LUAU'
    local opens, runs, logs, patches, requests, timers = {}, {}, {}, {}, {}, {}

    local ui = {
        row = function(props)
            props.kind = "row"
            return props
        end,
        column = function(props)
            props.kind = "column"
            return props
        end,
        popout = "popout",
    }

    local omniwm = {
        open = function(url)
            opens[#opens + 1] = url
        end,
        run = function(command)
            runs[#runs + 1] = command
        end,
        log = function(message)
            logs[#logs + 1] = message
        end,
        after = function(seconds, callback)
            timers[#timers + 1] = { seconds = seconds, callback = callback }
            return { cancel = function() end }
        end,
        http = function()
            error("the test must install omniwm.http before polling")
        end,
    }

    local function check(got, want, label)
        if got ~= want then
            error(string.format("%s: got %s, want %s", label, tostring(got), tostring(want)), 0)
        end
    end

    local function context(values)
        return {
            data = { settings = values, bar = { style = { height = 24, vertical = false } } },
            state = {},
        }
    end

    local function thread(kind, title, path, reason)
        return {
            reason = reason,
            url = "https://api.github.com/notifications/threads/1",
            subject = {
                type = kind,
                title = title,
                url = "https://api.github.com/repos/yuanw/nix-home" .. path,
            },
            repository = { name = "yuanw/nix-home", html_url = "https://github.com/yuanw/nix-home" },
        }
    end
    LUAU

    cat > tail.luau <<'LUAU'
    local notify = require("./notify")
    local REPO = "https://github.com/yuanw/nix-home"

    -- the decisions notify.luau makes on its own
    do
        local pull = thread("PullRequest", "Fix the build", "/pulls/42")
        check(notify.row(pull).url, REPO .. "/pull/42", "pull request url")
        check(notify.row(pull, { html_url = REPO .. "/pull/42/files" }).url,
            REPO .. "/pull/42/files", "fetched pull request html_url wins")
        check(notify.row(pull).icon, "arrow.triangle.pull", "pull request icon")
        check(notify.row(thread("Issue", "Boom", "/issues/7")).url, REPO .. "/issues/7", "issue url")
        check(notify.row(thread("Issue", "Boom", "/issues/7")).color, "#a6da95", "issue colour")
        check(notify.row(thread("Commit", "Rewrite it", "/commits/deadbeef")).url,
            REPO .. "/commit/deadbeef", "commit url")
        check(notify.row(thread("Discussion", "How?", "/discussions/3")).url, notify.INBOX, "discussion url")
        local suite = thread("CheckSuite", "ci failed", "/check-suites/9")
        check(notify.row(suite).url, REPO, "unknown subject type falls back to the repository")
        check(notify.row(suite).icon, "circle", "unknown subject type icon")

        local orphan = { subject = { type = "Issue", url = "https://api.github.com/x/issues/1" } }
        check(notify.row(orphan).url, notify.INBOX, "thread without a repository")
        check(notify.row(orphan).title, "", "thread without a title")
        check(notify.row(orphan).repo, "", "thread without a repository name")

        check(notify.important("Deprecate the old flag"), true, "deprecation is important")
        check(notify.important("This BREAKS the API"), true, "breaking is important")
        check(notify.important("It broke earlier today"), true, "broke is important")
        check(notify.important("Add the github widget"), false, "an ordinary title is not important")

        check(notify.decide(thread("Issue", "Boom", "/issues/7"), nil), "keep", "issues are kept")
        check(notify.decide(pull, "open"), "keep", "open pull requests are kept")
        check(notify.decide(pull, "closed"), "dismiss", "closed pull requests are dismissed")
        check(notify.decide(pull, nil), "keep", "an unread pull request state is kept")
        check(notify.decide(thread("PullRequest", "Fix", "/pulls/42", "team_mention"), "open"),
            "dismiss", "team mentions on pull requests are dismissed")
        check(notify.decide(thread("Discussion", "How?", "/discussions/3", "team_mention"), nil),
            "keep", "team mentions on other subjects are kept")
    end

    -- one poll over an inbox with an open pull request, a closed one, a team
    -- mention and an issue, then the tree that poll produced
    do
        local openPull = thread("PullRequest", "Add the widget", "/pulls/42")
        local closedPull = thread("PullRequest", "Abandoned idea", "/pulls/43")
        local mention = thread("PullRequest", "Look at this", "/pulls/44", "team_mention")
        local issue = thread("Issue", "Deprecate the old flag", "/issues/7")
        local pulls = {
            ["https://api.github.com/repos/yuanw/nix-home/pulls/42"] = {
                state = "open",
                html_url = REPO .. "/pull/42",
            },
            ["https://api.github.com/repos/yuanw/nix-home/pulls/43"] = { state = "closed" },
        }
        requests = {}
        patches = {}
        timers = {}
        opens = {}
        runs = {}
        omniwm.http = function(request, callback)
            requests[#requests + 1] = request.url
            local pull = pulls[request.url]
            if request.method == "PATCH" then
                patches[#patches + 1] = request.url
            elseif pull then
                callback({ ok = true, status = 200, headers = {}, json = pull })
            elseif string.find(request.url, "/notifications%?") then
                callback({
                    ok = true,
                    status = 200,
                    headers = {},
                    json = { openPull, closedPull, mention, issue },
                })
            else
                callback({ ok = false, status = 404, headers = {}, error = "http_status" })
            end
            return {}
        end

        local ctx = context({ token = "secret", interval = 120, dismissClosed = true })
        check(requests[1], nil, "nothing is fetched before the host triggers the widget")
        trigger(ctx, { name = "reloaded", source = "host" })

        local items = ctx.state.items
        check(#items, 2, "rows after one poll")
        check(items[1].url, REPO .. "/pull/42", "the open pull request kept its page")
        check(items[2].title, "Deprecate the old flag", "the issue was kept")
        check(ctx.state.important, true, "the breaking issue title reddened the bell")
        check(ctx.state.pulse, 1, "the count grew, so the bell bounced")
        check(#patches, 2, "the closed pull request and the team mention were marked read")
        check(#timers, 1, "one timer is queued")
        check(timers[1].seconds, 120, "the option chose the poll interval")

        local button = render(ctx)
        check(button.kind, "bar", "the button is a bar element")
        check(button.onClick, "popout", "the button opens the popout")
        check(button[1].name, "bell.badge.fill", "the bell with unread mail")
        check(button[1].color, "#ed8796", "red while something important waits")
        check(button[1].pulse.key, "github-1", "the pulse key the bounce animates on")
        check(button[2].text, "2", "the count on the bar")
        check(button[2].reserve, "88", "the count keeps its width")

        local panel = popout(ctx)
        check(panel.kind, "panel", "the popout is a panel")
        check(panel[1][2].name, "arrow.clockwise", "the panel offers a refresh")
        check(panel[1][2].action ~= nil, true, "refresh is clickable when no poll is running")
        check(#panel, 3, "heading and the two rows")
        local row = panel[2]
        check(row[2].text, "yuanw/nix-home", "the row names the repository")
        check(row[3].text, "Add the widget", "the row names the thread")
        row.onClick()
        check(opens[1], REPO .. "/pull/42", "the row opened its page")
        check(runs[1], "shell toggle github", "the row closed the popout")

        -- the next timer polls again and an unchanged inbox stays quiet
        timers[1].callback()
        check(ctx.state.pulse, 1, "the same count does not bounce the bell again")
    end

    -- a full page asks for the next one
    do
        local first, second = {}, {}
        for index = 1, 100 do
            first[index] = thread("Issue", "Issue " .. index, "/issues/" .. index)
        end
        second = { thread("Issue", "Last", "/issues/101") }
        local pages = 0
        omniwm.http = function(request, callback)
            pages += 1
            callback({
                ok = true,
                status = 200,
                headers = {},
                json = if pages == 1 then first else second,
            })
            return {}
        end
        local ctx = context({ token = "secret", interval = 60 })
        trigger(ctx, { name = "reloaded", source = "host" })
        check(pages, 2, "a full page asked for the next one")
        check(#ctx.state.items, 101, "both pages became rows")
        check(MAX_ROWS, 12, "the panel caps its rows below the row count")
        local panel = popout(ctx)
        check(#panel, 14, "heading, twelve rows and the rest of the inbox")
    end

    -- no token, so no request and a message instead of a count
    do
        requests = {}
        timers = {}
        local ctx = context({ token = "", interval = 60 })
        trigger(ctx, { name = "reloaded", source = "host" })
        check(#requests, 0, "a widget without a token asks GitHub nothing")
        check(ctx.state.error, "Set a GitHub token in this widget's options", "it says why")
        check(render(ctx)[2].text, "0", "and the bar reads zero")
        check(#timers, 1, "and it asks again at the interval, once a token shows up")
    end

    -- a GitHub that answers 401 keeps the last rows and says so
    do
        timers = {}
        omniwm.http = function(request, callback)
            callback({ ok = false, status = 401, headers = {} })
            return {}
        end
        local ctx = context({ token = "secret", interval = 60 })
        trigger(ctx, { name = "reloaded", source = "host" })
        check(ctx.state.items, nil, "a failed inbox fetch keeps the rows empty")
        check(ctx.state.error, "GitHub returned 401", "it reports the status")
        check(#timers, 1, "a failed poll still queues the next one")
    end

    print("github widget: every assertion passed")
    LUAU

    cat header.luau widgets/github/init.luau tail.luau > widgets/github/combined.luau
    luau widgets/github/combined.luau
    touch $out
  ''
