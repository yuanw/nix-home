{ pkgs }:

# The GitHub bar widget (modules/wm/omniwm/widgets/github) and its publisher
# (modules/wm/omniwm/github-inbox.sh), tested without OmniWM: the widget runs
# under pkgs.luau against fakes, and the publisher runs against a stubbed
# omniwmctl with the real jq and python3.
#
# look.luau stands in for the style's lib/look.luau, and json for the host's
# decoder -- the widget only ever decodes what the publisher published, so
# those two payloads are enough. The 4096 byte value bound the publisher trims
# to is omniwmctl's own (IPCShellRequest.valueLimit).
pkgs.runCommand "github-widget-tests"
  {
    nativeBuildInputs = [
      pkgs.luau
      pkgs.jq
      pkgs.python3
    ];
    meta.description = "Regression tests for the OmniWM GitHub widget and its inbox publisher";
  }
  ''
    set -euo pipefail
    mkdir -p widgets/github stub-bin

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
    return look
    LUAU

    cat > header.luau <<'LUAU'
    local opens, runs, decoded = {}, {}, {}

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
    }

    local json = {
        decode = function(raw)
            return decoded[raw]
        end,
    }

    local function check(got, want, label)
        if got ~= want then
            error(string.format("%s: got %s, want %s", label, tostring(got), tostring(want)), 0)
        end
    end

    local function context(published)
        local vars = {}
        if published ~= nil then
            vars["github-inbox"] = published
        end
        return {
            data = { vars = vars, bar = { style = { height = 24, vertical = false } } },
            state = {},
        }
    end

    local REPO = "https://github.com/yuanw/nix-home"
    local API = "https://api.github.com/repos/yuanw/nix-home"

    local function item(kind, title, path, page)
        return {
            r = "yuanw/nix-home",
            h = REPO,
            t = title,
            k = kind,
            s = API .. path,
            p = page or "",
        }
    end
    LUAU

    cat > tail.luau <<'LUAU'
    local notify = require("./notify")

    -- the page and the colours notify.luau decides from a published thread
    do
        local pull = item("PullRequest", "Add the widget", "/pulls/42")
        check(notify.rows({ i = { pull } })[1].url, REPO .. "/pull/42", "pull request url")
        check(notify.rows({ i = { item("PullRequest", "x", "/pulls/42", REPO .. "/pull/42/files") } })[1].url,
            REPO .. "/pull/42/files", "the published pull request page wins")
        check(notify.rows({ i = { pull } })[1].icon, "arrow.triangle.pull", "pull request icon")
        local issue = notify.rows({ i = { item("Issue", "Boom", "/issues/7") } })[1]
        check(issue.url, REPO .. "/issues/7", "issue url")
        check(issue.color, "#a6da95", "issue colour")
        check(notify.rows({ i = { item("Commit", "Rewrite it", "/commits/deadbeef") } })[1].url,
            REPO .. "/commit/deadbeef", "commit url")
        check(notify.rows({ i = { item("Discussion", "How?", "/discussions/3") } })[1].url,
            notify.INBOX, "discussion url")
        local suite = notify.rows({ i = { item("CheckSuite", "ci failed", "/check-suites/9") } })[1]
        check(suite.url, REPO, "an unknown subject type opens the repository")
        check(suite.icon, "circle", "an unknown subject type icon")

        local orphan = { r = nil, h = nil, t = nil, k = "Issue", s = API .. "/issues/1", p = nil }
        check(notify.rows({ i = { orphan } })[1].url, notify.INBOX, "a thread without a repository")
        check(notify.rows({ i = { orphan } })[1].title, "", "a thread without a title")
        check(#notify.rows({ i = { "junk", 1, pull } }), 1, "a published entry that is not a thread is skipped")
        check(#notify.rows({}), 0, "a payload without items")

        check(notify.important("Deprecate the old flag"), true, "deprecation is important")
        check(notify.important("This BREAKS the API"), true, "breaking is important")
        check(notify.important("It broke earlier today"), true, "broke is important")
        check(notify.important("Add the github widget"), false, "an ordinary title is not important")
    end

    -- what the bar and the popout make of a published inbox
    do
        decoded["two"] = {
            n = 2,
            i = {
                item("PullRequest", "Add the widget", "/pulls/42", REPO .. "/pull/42"),
                item("Issue", "Deprecate the old flag", "/issues/7"),
            },
        }
        local ctx = context("two")
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
        check(#panel, 3, "heading and the two rows")
        check(panel[2][2].text, "yuanw/nix-home", "the row names the repository")
        check(panel[2][3].text, "Add the widget", "the row names the thread")
        panel[2].onClick()
        check(opens[1], REPO .. "/pull/42", "the row opened its page")
        check(runs[1], "shell toggle github", "the row closed the popout")

        render(ctx)
        check(ctx.state.pulse, 1, "repainting the same inbox does not bounce the bell again")

        -- the count is the published one, which is more than the trimmed rows
        local many = {}
        for index = 1, 12 do
            many[index] = item("Issue", "Issue " .. index, "/issues/" .. index)
        end
        decoded["many"] = { n = 40, i = many }
        local busy = context("many")
        check(render(busy)[2].text, "40", "the bar counts the whole inbox")
        local busyPanel = popout(busy)
        check(#busyPanel, 14, "heading, twelve rows and the rest of the inbox")
        check(busyPanel[14].text:match("^28 more"), "28 more", "it says how many more there are")
    end

    -- a publisher that failed, and one that has not run since OmniWM started
    do
        decoded["broken"] = { n = 0, e = "gh api notifications failed", i = {} }
        local failed = context("broken")
        check(render(failed)[2].text, "0", "a failed publisher reads zero")
        check(popout(failed)[2].text, "gh api notifications failed", "and the popout says why")

        local waiting = context(nil)
        check(render(waiting)[2].text, "0", "no published inbox reads zero")
        check(popout(waiting)[2].text, "Waiting for the first inbox update", "and names the reason")

        decoded["junk"] = nil
        local junk = context("junk")
        check(render(junk)[2].text, "0", "a value that is not JSON reads zero")
        check(popout(junk)[2].text, "The published inbox is not JSON", "and says so")
    end

    print("github widget: every assertion passed")
    LUAU

    cat header.luau widgets/github/init.luau tail.luau > widgets/github/combined.luau
    luau widgets/github/combined.luau

    # the publisher: shape, the 4096 byte trim, and the failure the bar shows
    cp ${./../modules/wm/omniwm/github-inbox.sh} github-inbox.sh
    cat > stub-bin/omniwmctl <<'SH'
    #!/bin/sh
    printf '%s' "$4" > "$PUBLISHED"
    SH
    chmod +x stub-bin/omniwmctl
    PATH="$PWD/stub-bin:$PATH"
    export PATH

    cat > filter_small.py <<'PY'
    import json
    print(json.dumps([{
        "repository": {"name": "nix-home", "full_name": "yuanw/nix-home", "html_url": "https://github.com/yuanw/nix-home"},
        "subject": {
            "title": "Update flake.lock",
            "type": "PullRequest",
            "url": "https://api.github.com/repos/yuanw/nix-home/pulls/7",
            "html_url": "https://github.com/yuanw/nix-home/pull/7",
        },
    }]))
    PY

    cat > filter_many.py <<'PY'
    import json
    def thread(number):
        return {
            "repository": {"name": "yuanw/nix-home", "html_url": "https://github.com/yuanw/nix-home"},
            "subject": {
                "title": "更新依赖 " * 30 + str(number),
                "type": "PullRequest",
                "url": "https://api.github.com/repos/yuanw/nix-home/pulls/%d" % number,
            },
        }
    print(json.dumps([thread(number) for number in range(40)]))
    PY

    cat > filter_broken.py <<'PY'
    import sys
    print("not json")
    sys.exit(1)
    PY

    FILTER=filter_small.py PUBLISHED=$PWD/small.json sh ./github-inbox.sh
    test "$(jq -r '.n' small.json)" = 1
    test "$(jq -r '.i | length' small.json)" = 1
    test "$(jq -r '.i[0].t' small.json)" = "Update flake.lock"
    test "$(jq -r '.i[0].r' small.json)" = "yuanw/nix-home"
    test "$(jq -r '.i[0].p' small.json)" = "https://github.com/yuanw/nix-home/pull/7"
    test "$(jq -r '.i[0].s' small.json)" = "https://api.github.com/repos/yuanw/nix-home/pulls/7"

    FILTER=filter_many.py PUBLISHED=$PWD/many.json sh ./github-inbox.sh
    rows=$(jq '.i | length' many.json)
    bytes=$(wc -c < many.json | tr -d ' ')
    test "$(jq -r '.n' many.json)" = 40
    test "$rows" -lt 12
    test "$bytes" -le 4096

    FILTER=filter_broken.py PUBLISHED=$PWD/broken.json sh ./github-inbox.sh
    test "$(jq -r '.e' broken.json)" = "gh api notifications failed"
    test "$(jq -r '.n' broken.json)" = 0

    touch $out
  ''
