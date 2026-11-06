-- MIT (c) 2026 Preston Peranich -- from pkgs/rex-project-picker in
-- https://github.com/pperanich/dotfiles at 29ea77f4, with one local change (the
-- "/bin" tool prefix below).  See rex.nix for how it is deployed.
local terminal = "com.superlogical.terminal"
local directory = debug.getinfo(1, "S").source:gsub("^@", ""):match("^(.*)/")
local options

local function tool(name)
	if options.tools and options.tools[name] then
		return options.tools[name]
	end
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
	error("rex-project-picker requires " .. name .. " in your Nix profile (or set tools." .. name .. ")", 0)
end

local function checked(result, err)
	if not result then
		error(tostring(err or "Rex returned no result"), 0)
	end
	return result
end

local function app_client(ctx)
	local apps = {}
	for _, client in ipairs(checked(rex.call("client.list")).clients) do
		if client.info and client.info.kind == "app" and client.connection_state == "connected" then
			if client.client_id == ctx.client_id then
				return client.client_id
			end
			apps[#apps + 1] = client.client_id
		end
	end
	if #apps ~= 1 then
		error("Choose one connected Rex app with -C (or the launcher's APP_CLIENT argument)", 0)
	end
	return apps[1]
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
	rex.action({
		name = "rex_project_picker",
		title = "rex-project-picker: Open Project…",
		run = function(ctx)
			assert(ctx.session_id, "Run this in an existing Rex session")
			local client = app_client(ctx)
			local command = {
				tool("bash"),
				directory .. "/rex-project-picker",
				"pick",
				tool("rex"),
				tool("fzf"),
				tool("fd"),
				client,
				ctx.server or "",
			}
			for _, root in ipairs(options.roots) do
				command[#command + 1] = root.path
				command[#command + 1] = tostring(root.depth)
			end
			local request = {
				session_id = ctx.session_id,
				bounds = options.bounds or { 0.1, 0.2, 0.8, 0.6 },
				focus = true,
				layout = {
					block = {
						flavor = terminal .. ".shell",
						options = {
							command = command,
							shell = "none",
							cwd = os.getenv("HOME"),
						},
					},
				},
			}
			-- Keep the existing prototype's CLI workaround for Beta's Lua layer launch.
			local argv = { tool("rex"), "--autostart=false", "--color=never" }
			if ctx.server and ctx.server ~= "" then
				argv[#argv + 1] = "-S"
				argv[#argv + 1] = ctx.server
			end
			for _, value in ipairs({ "api", "call", "session.new_layer", checked(rex.json.encode(request)) }) do
				argv[#argv + 1] = value
			end
			local process = checked(rex.exec(argv, { timeout = 10 }))
			if not process.ok then
				error("Open popup: " .. process.stderr, 0)
			end
			return checked(rex.json.decode(process.stdout))
		end,
	})

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

	rex.action({
		name = "rex_project_picker_open",
		title = "rex-project-picker: Open Directory",
		args = { path = "string" },
		run = function(ctx, args)
			local path = assert(canonical(args.path), "Project directory is unavailable")
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
		end,
	})
end

return { setup = setup }
