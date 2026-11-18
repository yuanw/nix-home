-- Self-check for picker.lua: no Rex, no fd, no h.  It stubs the rex API, drives
-- the picker action through the answers the palette would give, and asserts on
-- what the picker asks for and what it opens.  Run it from the repo root:
--   nix run nixpkgs#lua5_1 -- modules/terminal-multiplexer/rex-project-picker/picker-selfcheck.lua
-- Rex embeds GopherLua, a Lua 5.1 dialect, hence 5.1; the checks also pass on
-- 5.4 and 5.5, so keep picker.lua free of anything newer than 5.1.
local here = debug.getinfo(1, "S").source:gsub("^@", ""):match("^(.*)/")

local calls = {}
local pick_answer -- what rex.ui.pick hands back: { id }, { { text = ... } }, or { nil, why }
local found -- what the helper's `list' prints, or { ok = false } to fail
local cloned -- resolve answers, keyed by spec
local git_handles_open -- when false, io.open refuses the .git marker

local function reset()
	calls = {}
	found = ""
	cloned = {}
	git_handles_open = true
end

local real_open = io.open
io.open = function(path, mode)
	if type(path) == "string" and path:sub(-5) == "/.git" then
		return git_handles_open and { close = function() end } or nil
	end
	return real_open(path, mode)
end

rex = {
	action = function(definition)
		rex.actions[definition.name] = definition
	end,
	actions = {},
	log = function(level, message)
		calls[#calls + 1] = { log = level .. ": " .. message }
	end,
	exec = function(argv, opts)
		calls[#calls + 1] = { exec = table.concat(argv, " "), opts = opts }
		local subcommand = argv[3]
		if subcommand == "list" then
			if type(found) == "table" then
				return { ok = false, code = 3, stdout = "", stderr = found.stderr }
			end
			return { ok = true, code = 0, stdout = found, stderr = "" }
		elseif subcommand == "resolve" then
			local answer = cloned[argv[6]]
			if not answer then
				return { ok = false, code = 1, stdout = "", stderr = "h could not resolve '" .. argv[6] .. "'." }
			end
			return { ok = true, code = 0, stdout = answer .. "\n", stderr = "Cloning into ..." }
		end
		-- canonical() asks `pwd -P' for a directory; echoing the cwd back keeps the
		-- paths under test recognizable.
		return { ok = true, code = 0, stdout = (opts and opts.cwd or "") .. "\n", stderr = "" }
	end,
	ui = {
		pick = function(request)
			calls[#calls + 1] = { pick = request }
			return pick_answer[1], pick_answer[2], pick_answer[3]
		end,
	},
	call = function(method, args)
		calls[#calls + 1] = { call = method, args = args }
		if method == "session.list" then
			return { sessions = {} }
		elseif method == "session.create" then
			return { session_id = "s-new", initial_windows = { { window_id = "w-new" } } }
		end
		error("unexpected rex.call " .. method, 0)
	end,
	client = {
		queue = function(name, args)
			calls[#calls + 1] = { queue = name, args = args }
		end,
	},
	block = { call = function() return nil end },
}

assert(loadfile(here .. "/picker.lua"))().setup({
	roots = { { path = "/home/me/workspaces", depth = 3 } },
	codeRoot = "/home/me/workspaces",
})

local function last(kind, fragment)
	for i = #calls, 1, -1 do
		local entry = calls[i]
		if entry[kind] and (fragment == nil or tostring(entry[kind]):find(fragment, 1, true)) then
			return entry
		end
	end
end

local function opened()
	local queued = last("queue")
	return queued and queued.args
end

local function check(name, condition)
	print((condition and "ok   " or "FAIL ") .. name)
	if not condition then
		os.exit(1)
	end
end

local function errors_with(run, fragment)
	local ok, err = pcall(run)
	return not ok and tostring(err):find(fragment, 1, true) ~= nil
end

-- Listing: NUL-separated paths become one folder item each, split into the
-- basename and its parent directory.
reset()
found = "/home/me/workspaces/github.com/yuanw/nix-home\0/home/me/workspaces/gitea.com/me/notes\0"
pick_answer = { "/home/me/workspaces/github.com/yuanw/nix-home" }
rex.actions.rex_project_picker.run()
local request = last("pick").pick
check("lists one item per project", #request.items == 2)
check("names the project", request.items[1].title == "nix-home")
check("shows where it lives", request.items[1].subtitle == "/home/me/workspaces/github.com/yuanw")
check("carries the path as its id", request.items[1].id == "/home/me/workspaces/github.com/yuanw/nix-home")
check("typed answers are accepted", request.allow_other == true)
check("filters on projects, not fruit", request.title == "Open project" and request.placeholder == "Filter projects")
check("asks fd for the configured root and depth", last("exec", "list /home/me/workspaces 3") ~= nil)
check("names fd by path, for the server's bare PATH", last("exec", "list ").opts.env.PROJECT_PICKER_FD:match("/fd$") ~= nil)
check("opens the chosen project", opened().session_id == "s-new")

-- A typed spec goes through h, and whatever h prints is what gets opened.
reset()
found = ""
cloned["owner/repo"] = "/home/me/workspaces/github.com/owner/repo"
pick_answer = { { text = "owner/repo" } }
rex.actions.rex_project_picker.run()
check("resolves a typed spec against codeRoot", last("exec", "resolve /home/me/workspaces ") ~= nil)
check("hands h the typed spec", last("exec", " owner/repo") ~= nil)
check("opens what h resolved", opened().session_id == "s-new" and last("call", "session.create").args.label == "repo")

-- No answer: cancelling is silent, anything else is logged.
reset()
pick_answer = { nil, "cancelled" }
check("cancelling opens nothing and says nothing", rex.actions.rex_project_picker.run() == nil
	and opened() == nil and last("log") == nil)
pick_answer = { nil, "no_client" }
rex.actions.rex_project_picker.run()
check("an unanswered prompt is logged", last("log").log:find("no_client") ~= nil and opened() == nil)

-- Guards: fd failing, h failing, too many projects, and a vanished directory.
reset()
found = { ok = false, stderr = "fd: permission denied" }
pick_answer = { "/any" }
check("reports fd failing", errors_with(rex.actions.rex_project_picker.run, "fd: permission denied"))

reset()
found = ""
pick_answer = { { text = "nobody/nope" } }
check("reports h failing", errors_with(rex.actions.rex_project_picker.run, "could not resolve"))

reset()
local many = {}
for i = 1, 2001 do
	many[#many + 1] = "/home/me/workspaces/github.com/me/r" .. i
end
found = table.concat(many, "\0")
pick_answer = { "/any" }
check("refuses to list past the palette's cap", errors_with(rex.actions.rex_project_picker.run, "2000"))

reset()
found = ""
pick_answer = { "/home/me/workspaces/github.com/me/gone" }
git_handles_open = false
check("reports a directory with no .git", errors_with(rex.actions.rex_project_picker.run, "no .git marker"))

print("picker-selfcheck: all checks passed")
