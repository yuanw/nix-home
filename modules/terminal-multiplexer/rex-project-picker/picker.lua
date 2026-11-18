-- MIT (c) 2026 Preston Peranich -- from pkgs/rex-project-picker in
-- https://github.com/pperanich/dotfiles at 29ea77f4, with local changes: the
-- "/bin" tool prefix, and a palette prompt (rex.ui.pick) in place of upstream's
-- fzf popup.  A typed answer -- repo name, owner/repo or git URL -- resolves
-- through h against codeRoot, cloning it first, which is what upstream's Ctrl-O
-- binding did.  See rex.nix for how it is deployed.
local terminal = "com.superlogical.terminal"
local directory = debug.getinfo(1, "S").source:gsub("^@", ""):match("^(.*)/")
local options

local function tool(name)
	local home = assert(os.getenv("HOME"))
	local user = assert(os.getenv("USER"))
	for _, prefix in ipairs({
		"/etc/profiles/per-user/" .. user,
		home .. "/.nix-profile",
		"/run/current-system/sw",
		"/bin", -- macOS bash and pwd live here, and upstream's list skips /bin
		"/usr",
		"",
	}) do
		local path = prefix .. "/bin/" .. name
		local file = io.open(path, "rb")
		if file then
			file:close()
			return path
		end
	end
	error("rex-project-picker requires " .. name .. " in your Nix profile", 0)
end

-- h is optional here: the picker lists projects without it, and a typed answer
-- then reports that cloning is not configured.  Keep it out of tool()'s error.
local function optional_tool(name)
	local ok, path = pcall(tool, name)
	return ok and path or ""
end

local function checked(result, err)
	if not result then
		error(tostring(err or "Rex returned no result"), 0)
	end
	return result
end

local function setup(config)
	options = config
	assert(type(options.roots) == "table" and #options.roots > 0, "Configure at least one project root")
	for _, root in ipairs(options.roots) do
		assert(type(root.path) == "string" and root.path:sub(1, 1) == "/", "Project roots must be absolute paths")
		assert(
			type(root.depth) == "number" and root.depth >= 0 and root.depth <= 32 and root.depth % 1 == 0,
			"Root depths must be integers in 0..32"
		)
	end

	local function canonical(path)
		if type(path) ~= "string" or path:sub(1, 1) ~= "/" then
			return nil
		end
		local process = rex.exec({ tool("pwd"), "-P" }, { cwd = path, timeout = 2 })
		if process and process.ok then
			return (process.stdout:gsub("\n$", ""))
		end
	end

	local function block_cwd(session, block)
		local args = { session_id = session, block_id = block.block_id }
		local process = rex.block.call(terminal, "process", args)
		local cwd = canonical(process and process.foreground and process.foreground.cwd)
		if cwd then
			return cwd
		end
		local pwd = rex.block.call(terminal, "pwd", args)
		local path = pwd and pwd.pwd
		if type(path) == "string" and path:sub(1, 5) == "file:" then
			local host, encoded = path:match("^file://([^/]*)(/.*)$")
			if not encoded or (host ~= "" and host ~= "localhost") then
				return nil
			end
			path = encoded:gsub("%%(%x%x)", function(hex)
				return string.char(tonumber(hex, 16))
			end)
		end
		return canonical(path)
	end

	-- Reused by both actions: the palette hands it a chosen path, and the CLI can
	-- still reach it with `rex do rex_project_picker_open path=...'.
	local function open_directory(requested)
		local path = assert(canonical(requested), "Project directory is unavailable")
		local marker = io.open(path .. "/.git", "r")
		assert(marker, "Selected directory has no .git marker")
		marker:close()
		local match
		for _, session in ipairs(checked(rex.call("session.list")).sessions or {}) do
			-- The attach helper exists in scripts, but this Beta's config engine needs rex.call.
			local view = checked(rex.call("session.attach", { session_id = session.session_id })).view
			local windows = {}
			for _, window in ipairs(view.windows or {}) do
				for _, layer in ipairs(window.layers or {}) do
					-- Floating picker terminals are excluded from project identity.
					if layer.kind == "tiled" then
						for _, block in ipairs(layer.blocks or {}) do
							if block.creator_name == terminal and block_cwd(session.session_id, block) == path then
								windows[window.window_id] = true
							end
						end
					end
				end
			end
			local window_id, count = nil, 0
			for id in pairs(windows) do
				window_id, count = id, count + 1
			end
			if count > 0 then
				assert(not match, "Multiple sessions match this project; use Rex's session switcher")
				assert(
					count == 1 or windows[view.active_window_id],
					"Multiple project tabs match; select one in Rex"
				)
				match = {
					session_id = session.session_id,
					window_id = windows[view.active_window_id] and view.active_window_id or window_id,
				}
			end
		end
		local reused = match ~= nil
		if not match then
			local created = checked(rex.call("session.create", {
				label = path:match("([^/]+)$"):gsub("%c", " "):sub(1, 100),
				initial_windows = {
					{
						window_label = "Terminal",
						layout = { block = { flavor = terminal .. ".shell", options = { cwd = path } } },
					},
				},
			}))
			match = { session_id = created.session_id, window_id = created.initial_windows[1].window_id }
		end
		rex.client.queue("session.select", match)
		return { session_id = match.session_id, window_id = match.window_id, reused = reused, path = path }
	end

	-- fd separates paths with NULs, so a path may hold a newline.  Splitting by hand
	-- rather than gmatch: a pattern holding a literal NUL is invalid in Lua 5.1,
	-- which is the dialect Rex's config engine speaks.
	local function split_nul(text)
		local paths, start, length = {}, 1, #text
		for index = 1, length + 1 do
			if index == length + 1 or text:byte(index) == 0 then
				if index > start then
					paths[#paths + 1] = text:sub(start, index - 1)
				end
				start = index + 1
			end
		end
		return paths
	end

	-- fd finds the .git markers; the config engine's Lua has no directory walk, so
	-- the helper does the finding and prints the paths separated by NULs.
	local function project_items()
		local command = { tool("bash"), directory .. "/rex-project-picker", "list" }
		for _, root in ipairs(options.roots) do
			command[#command + 1] = root.path
			command[#command + 1] = tostring(root.depth)
		end
		local found = checked(
			rex.exec(command, { timeout = 30, env = { PROJECT_PICKER_FD = tool("fd") } }),
			"Cannot run the project picker helper"
		)
		if not found.ok then
			error("List projects: " .. (found.stderr ~= "" and found.stderr or "fd exited " .. tostring(found.code)), 0)
		end
		local items = {}
		for _, path in ipairs(split_nul(found.stdout)) do
			items[#items + 1] = {
				id = path,
				title = path:match("([^/]+)$"),
				subtitle = path:match("^(.*)/"),
				icon = "folder",
			}
		end
		-- A prompt holds at most 2000 items.  Naming the count beats Rex rejecting
		-- the call, and points at the fix.
		if #items > 2000 then
			error(("%d projects match the configured roots; Rex lists 2000 at most, so narrow them"):format(#items), 0)
		end
		return items
	end

	-- The helper only trusts h's last stdout line and clones on stderr, which is
	-- slow enough on a large repo to want more than rex.exec's default patience.
	local function resolve_spec(text)
		local cloned = checked(
			rex.exec({
				tool("bash"),
				directory .. "/rex-project-picker",
				"resolve",
				options.codeRoot or "",
				optional_tool("h"),
				text,
			}, { timeout = 600 }),
			"Cannot run the project picker helper"
		)
		if not cloned.ok then
			error("Open typed project: " .. (cloned.stderr ~= "" and cloned.stderr or "h exited " .. tostring(cloned.code)), 0)
		end
		return (cloned.stdout:gsub("%s+$", ""))
	end

	rex.action({
		name = "rex_project_picker",
		title = "rex-project-picker: Open Project…",
		run = function()
			-- The palette asks every connected app and the first answer wins, so this
			-- needs no client of its own and no round trip back through the CLI.
			-- Keep it out of pcall: rex.ui.pick refuses to ask from one.
			local choice, why, detail = rex.ui.pick({
				title = "Open project",
				placeholder = "Filter projects",
				items = project_items(),
				-- The typed answer is how a repo that is not cloned yet gets opened.
				allow_other = true,
				-- The 300s default closes the palette on someone still deciding.
				timeout = 900,
			})
			if choice == nil then
				if why ~= "cancelled" then
					rex.log("warn", "rex-project-picker: " .. why .. " " .. (detail or ""))
				end
				return
			end
			local path = choice
			if type(choice) == "table" then
				path = resolve_spec(choice.text)
			end
			return open_directory(path)
		end,
	})

	rex.action({
		name = "rex_project_picker_open",
		title = "rex-project-picker: Open Directory",
		args = { path = "string" },
		run = function(_, args)
			return open_directory(args.path)
		end,
	})
end

return { setup = setup }
