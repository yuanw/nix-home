-- pi-resurrect: remember which pi agent runs in which Rex pane, and with
-- which pi session, so that the agents can be resumed in the same panes after
-- the Rex server restarts.
--
-- One file, two roles:
--
--   * Loaded from init.lua (`dofile(path).setup{}`), it defines the
--     pi_resurrect_* actions and starts the tracker in the background.
--   * Run with `rex do pi-resurrect.lua [cmd=...]`, it is the tracker itself
--     (cmd=daemon) or a one-shot command (status, scan, restore, stop,
--     selftest).
--
-- Rex restores sessions, windows and panes with their IDs on restart, but
-- each pane comes back as a fresh shell. The tracker keeps a state file that
-- maps block ID -> agent + agent session ID + cwd. After a restart it types
-- the agent's resume command into each of those panes.
--
-- Adapted from github.com/ekristen/rex-resurrect at a1e41e291615cd05e06bcc118931b5c4b4f6afe9,
-- keeping its tracker, restore, and what-is-typed-into-a-shell rules, and
-- replacing its three agents with pi alone.

local MODNAME = ...

local M = {}

local HOME = os.getenv("HOME") or ""
local TERMINAL = "com.superlogical.terminal"
local UUID = "%x%x%x%x%x%x%x%x%-%x%x%x%x%-%x%x%x%x%-%x%x%x%x%-%x%x%x%x%x%x%x%x%x%x%x%x"

local SHELLS = {
  zsh = true, bash = true, fish = true, sh = true, dash = true, ksh = true,
  nu = true, tcsh = true, csh = true, xonsh = true, elvish = true, pwsh = true,
}

local defaults = {
  -- Where state.json, daemon.pid, daemon.log and last-restore.json live.
  state_dir = (os.getenv("XDG_STATE_HOME") or (HOME .. "/.local/state")) .. "/pi-resurrect",
  -- Path of the rex binary. Found from the running server when nil.
  rex_bin = nil,
  -- Seconds between full scans. These catch a session changed from inside a
  -- running agent, such as /resume or /new, which Rex has no event for.
  scan_interval = 10,
  -- Seconds after a process starts or exits until the scan it triggers, and
  -- until the second scan that follows it.
  event_delay = 1,
  followup_delay = 10,
  -- While an agent seen less than id_retry_window seconds ago has no session
  -- ID yet, scan again every id_retry_interval seconds.
  id_retry_interval = 2,
  id_retry_window = 60,
  -- Seconds an agent may be gone from its pane before it is forgotten. It
  -- must outlast a server shutdown, where agents die before the tracker does.
  grace = 90,
  -- Resume agents on their own when the tracker sees a new server instance.
  auto_restore = true,
  -- Seconds to wait after a restart before typing into the restored panes.
  restore_delay = 3,
  -- Open a new tab for an agent whose pane no longer exists.
  reopen_missing = false,
  -- Seconds back a session file's last write may be and still count as one pi
  -- is appending to now, which is how a session switched with /new or /clone
  -- is told from the one it replaced.
  active_window = 60,
  -- Session files read per directory per scan, newest first. Only the header
  -- line of each is read.
  candidate_read_limit = 12,
  -- The command typed to resume each agent. {id} is the agent's session ID.
  -- `names` lists the program names that identify the agent. `env` lists
  -- the variables that select the agent's profile: the directory holding its
  -- configuration and sessions. Their values are recorded from the running
  -- agent and set again in front of the resume command.
  agents = {
    pi = {
      names = { "pi" },
      env = { "PI_CODING_AGENT_DIR", "PI_CODING_AGENT_SESSION_DIR" },
      resume = "pi --session {id}",
    },
  },
}

---------------------------------------------------------------------------
-- Small helpers

local function merge(base, over)
  local out = {}
  for k, v in pairs(base) do
    out[k] = type(v) == "table" and merge(v, {}) or v
  end
  for k, v in pairs(over or {}) do
    if type(v) == "table" and type(out[k]) == "table" and #v == 0 then
      out[k] = merge(out[k], v)
    else
      out[k] = v
    end
  end
  return out
end

local function basename(path)
  if type(path) ~= "string" then return nil end
  return (path:gsub("/+$", ""):match("([^/]+)$"))
end

local function read_file(path)
  local f = io.open(path, "r")
  if not f then return nil end
  local text = f:read("*a")
  f:close()
  return text
end

local function write_file(path, text)
  local tmp = path .. ".tmp"
  local f, err = io.open(tmp, "w")
  if not f then return nil, err end
  f:write(text)
  f:close()
  return os.rename(tmp, path)
end

local function exec(argv, opts)
  local r = rex.exec(argv, opts or { timeout = 10 })
  if r == nil or not r.ok then return nil end
  return r.stdout
end

local function shquote(s)
  return "'" .. tostring(s):gsub("'", "'\\''") .. "'"
end

local function stamp(t)
  return os.date("!%Y-%m-%dT%H:%M:%SZ", t or os.time())
end

local log_path
local function log(fmt, ...)
  local line = stamp() .. " " .. string.format(fmt, ...)
  if log_path then
    local f = io.open(log_path, "a")
    if f then
      f:write(line, "\n")
      f:close()
      return
    end
  end
  print(line)
end

---------------------------------------------------------------------------
-- Finding an agent's session ID from its process

local function process_args(pid)
  return exec({ "ps", "-o", "args=", "-p", tostring(pid) })
end

-- A session named on the command line, as in `pi --session ID`, `pi --fork ID`
-- or `pi --session path/to/<created>_<id>.jsonl`. The fallback for a session
-- whose file the tracker cannot find.
local function id_from_args(pid)
  local args = process_args(pid)
  if not args then return nil end
  local id = args:match("%-%-session[%s=](" .. UUID .. ")")
    or args:match("%-%-fork[%s=](" .. UUID .. ")")
  if id then return id end
  local path = args:match("%-%-session[%s=](%S+%.jsonl)")
  return path and path:match("(" .. UUID .. ")%.jsonl$")
end

-- One environment variable of a process, or nil. An agent's own environment
-- can differ from the tracker's: direnv, for one, may point an agent's profile
-- directory at another value per project. A value with a space in it is cut
-- short.
local function process_env(pid, name)
  if type(name) ~= "string" or not name:match("^[%a_][%w_]*$") then return nil end
  local f = io.open("/proc/" .. pid .. "/environ", "rb")
  if f then
    local env = "\0" .. (f:read("*a") or "")
    f:close()
    return env:match("%z" .. name .. "=([^%z]*)")
  end
  local out = exec({ "ps", "eww", "-o", "command=", "-p", tostring(pid) })
  return out and (" " .. out):match(" " .. name .. "=(%S+)")
end

---------------------------------------------------------------------------
-- Finding the session a pane's pi holds open
--
-- pi records each session in a JSONL file named <created>_<id>.jsonl under
-- <agent dir>/sessions, in a directory named after the working directory the
-- session was made in. It appends to that file and closes it between writes,
-- so lsof cannot name it; the directory and the file headers say who owns it.

local function absolute(path)
  return type(path) == "string" and path:sub(1, 1) == "/" and path or nil
end

-- Where pi keeps its sessions: --session-dir, then PI_CODING_AGENT_SESSION_DIR,
-- then <agent dir>/sessions. A relative one is pi's own to resolve against its
-- working directory, which the tracker cannot read, so it counts as unset. The
-- sessionDir setting is not read here; it is the one way to hide a session
-- from the tracker.
local function sessions_root(pid)
  local args = process_args(pid) or ""
  return absolute(args:match("%-%-session%-dir[%s=](%S+)"))
    or absolute(process_env(pid, "PI_CODING_AGENT_SESSION_DIR"))
    or absolute(os.getenv("PI_CODING_AGENT_SESSION_DIR"))
    or ((absolute(process_env(pid, "PI_CODING_AGENT_DIR") or os.getenv("PI_CODING_AGENT_DIR"))
      or (HOME .. "/.pi/agent")) .. "/sessions")
end

-- The directory pi names for a working directory: no leading slash, every
-- character outside [A-Za-z0-9._-] replaced by a dash, wrapped in "--".
local function sessions_dir_for(root, cwd)
  return root .. "/--" .. ((cwd:gsub("^/+", "")):gsub("[^%w%._%-]", "-")) .. "--"
end

-- The header line of a session file, { id =, cwd = }, or nil.
local function session_header(path)
  local f = io.open(path, "r")
  if not f then return nil end
  local line = f:read("*l")
  f:close()
  local head = line and rex.json.decode(line)
  if type(head) ~= "table" or head.type ~= "session" or type(head.id) ~= "string" then
    return nil
  end
  return head
end

-- The names of the session files written in the last `minutes` minutes.
local function freshly_written(dir, minutes)
  local out = exec({
    "sh", "-c",
    'cd -- "$1" 2>/dev/null && find . -maxdepth 1 -name "*.jsonl" -mmin -"$2" -print 2>/dev/null',
    "sh", dir, tostring(minutes),
  })
  local fresh = {}
  for line in (out or ""):gmatch("[^\n]+") do
    fresh[line:gsub("^%./", "")] = true
  end
  return fresh
end

-- pi's session files for one working directory, newest first, as
-- { id =, fresh = }. `fresh` marks a file written in the last
-- opts.active_window seconds, which tells the session pi is appending to now
-- from the one it appended to a minute ago. Files are read newest first, and
-- a directory can hold two working directories (pi spells `a/b' and `a-b' the
-- same), so the header, not the name, decides whose file it is.
local function session_files(dir, cwd, opts, fresh)
  local ordered = exec({
    "sh", "-c",
    'cd -- "$1" 2>/dev/null && ls -1t -- *.jsonl 2>/dev/null',
    "sh", dir,
  })
  local found, read = {}, 0
  for name in (ordered or ""):gmatch("[^\n]+") do
    if read >= opts.candidate_read_limit then break end
    read = read + 1
    local head = session_header(dir .. "/" .. name)
    local id = name:match("^[%d%-%a]+_(" .. UUID .. ")%.jsonl$")
    if head and id == head.id and head.cwd == cwd then
      found[#found + 1] = { id = head.id, fresh = fresh[name] or false }
    end
  end
  return found
end

-- The same, from every directory, for the case where pi named the working
-- directory another way. Only files written recently are read, so a sessions
-- tree of thousands of files costs one find.
local function session_files_recent(root, cwd, opts, minutes)
  local out = exec({
    "sh", "-c",
    'find "$1" -maxdepth 2 -name "*.jsonl" -mmin -"$2" -print 2>/dev/null',
    "sh", root, tostring(minutes),
  })
  local paths = {}
  for line in (out or ""):gmatch("[^\n]+") do paths[#paths + 1] = line end
  -- The name begins with the time the session started, so descending names
  -- are descending start times.
  table.sort(paths, function(a, b) return a > b end)
  local found = {}
  for i = 1, math.min(#paths, opts.candidate_read_limit) do
    local head = session_header(paths[i])
    local id = head and paths[i]:match("^.*(" .. UUID .. ")%.jsonl$")
    if head and id == head.id and head.cwd == cwd then
      found[#found + 1] = { id = head.id, fresh = true }
    end
  end
  return found
end

-- Hand each pane its own session. Two panes in one directory share a
-- candidate list, and one session resumed in two panes at once would have
-- both writing it, so every file is handed out once. A pane that has no ID
-- yet takes a file pi is writing now; a pane keeps the ID the last scan
-- recorded, unless some other pane has a file being written that nobody
-- claimed -- that is a /new or /clone, whose predecessor must be given up.
-- What is left goes newest first, to the newest pane first. `candidates` are
-- newest first, as { id =, fresh = }.
local function assign(items, candidates)
  table.sort(items, function(a, b)
    return (a.entry.first_seen or 0) > (b.entry.first_seen or 0)
  end)

  local claimed, used = {}, {}
  local held_by_someone = {}
  for _, item in ipairs(items) do
    if item.entry.agent_session_id then held_by_someone[item.entry.agent_session_id] = true end
  end
  local orphan_fresh = false
  for _, c in ipairs(candidates) do
    if c.fresh and not held_by_someone[c.id] then orphan_fresh = true end
  end

  local function give(pass)
    for _, item in ipairs(items) do
      if not claimed[item] then
        local held = item.entry.agent_session_id
        for _, c in ipairs(candidates) do
          local want
          if pass == 1 then want = held == nil and c.fresh
          elseif pass == 2 then want = c.id == held and c.fresh
          elseif pass == 3 then want = c.id == held and not c.fresh and not orphan_fresh
          else want = true end
          if want and not used[c.id] then
            used[c.id] = true
            claimed[item] = true
            item.entry.agent_session_id = c.id
            break
          end
        end
      end
    end
  end
  give(1)
  give(2)
  give(3)
  give(4)
end

-- Read every pane's session ID, grouping the panes by working directory so
-- each pane is weighed against the others that could claim the same file.
local function resolve_ids(found, opts)
  local minutes = math.ceil(opts.active_window / 60)
  local by_cwd = {}
  for _, item in ipairs(found) do
    if item.cwd then
      local list = by_cwd[item.cwd]
      if not list then list = {}; by_cwd[item.cwd] = list end
      list[#list + 1] = item
    end
  end

  for cwd, items in pairs(by_cwd) do
    local root = sessions_root(items[1].pid)
    local dir = sessions_dir_for(root, cwd)
    local candidates = session_files(dir, cwd, opts, freshly_written(dir, minutes))
    if #candidates == 0 then
      candidates = session_files_recent(root, cwd, opts, minutes)
    end
    assign(items, candidates)

    -- Nothing to read (a session pi has not written yet, an ephemeral one),
    -- so fall back to an ID the command line named.
    for _, item in ipairs(items) do
      if not item.entry.agent_session_id then
        item.entry.agent_session_id = id_from_args(item.pid)
      end
    end
  end
end

-- The agent running in a terminal, from the terminal's `process` result.
-- Returns the agent's name and its process, or nil.
local function detect(proc, opts)
  local candidates = {}
  if type(proc.foreground) == "table" then candidates[#candidates + 1] = proc.foreground end
  for _, p in ipairs(proc.ancestors or {}) do candidates[#candidates + 1] = p end

  for _, p in ipairs(candidates) do
    local names = {}
    for _, field in ipairs({ "argv0", "invoked_path", "script", "name", "path" }) do
      local n = basename(p[field])
      if n then names[n:gsub("^%-", "")] = true end
    end
    for agent, spec in pairs(opts.agents) do
      for _, n in ipairs(spec.names or { agent }) do
        if names[n] then return agent, p end
      end
    end
  end
  return nil
end

---------------------------------------------------------------------------
-- Calling Rex. A script must be attached to a session to query it.

local function with_attach(session_id, fn)
  local r, err = fn()
  if r == nil and tostring(err):find("not attached", 1, true) then
    rex.session.attach{ session_id = session_id }
    r, err = fn()
  end
  return r, err
end

local function scall(method, session_id, args)
  args = args or {}
  args.session_id = session_id
  return with_attach(session_id, function() return rex.call(method, args) end)
end

local function bcall(method, session_id, block_id, args)
  args = args or {}
  args.session_id = session_id
  args.block_id = block_id
  return with_attach(session_id, function() return rex.block.call(TERMINAL, method, args) end)
end

---------------------------------------------------------------------------
-- State

local function state_path(opts) return opts.state_dir .. "/state.json" end

-- Whether a file or directory belongs to the user running this.
local function owned(path)
  return exec({ "test", "-O", path }) ~= nil
end

-- The state decides what is typed into panes, so it is read only from a
-- file and directory this user owns. Anything else loads as empty.
local function load_state(opts)
  local state
  local text = read_file(state_path(opts))
  if text then
    if owned(opts.state_dir) and owned(state_path(opts)) then
      state = rex.json.decode(text)
    else
      rex.log("warn", "pi-resurrect: ignoring " .. state_path(opts) .. ": not owned by this user")
    end
  end
  if type(state) ~= "table" then state = {} end
  if type(state.entries) ~= "table" then state.entries = {} end
  for block_id, e in pairs(state.entries) do
    if type(block_id) ~= "string" or type(e) ~= "table" then state.entries[block_id] = nil end
  end
  state.version = 1
  return state
end

local function save_state(opts, state)
  state.heartbeat = os.time()
  local text = rex.json.encode(state, { indent = true })
  if not text then return nil, "cannot encode state" end
  return write_file(state_path(opts), text)
end

-- Look at every terminal on the server and bring state.entries up to date.
-- Returns true, or nil and a message when the server could not be asked. A
-- session or block that fails to answer keeps its entries untouched.
local function scan(state, opts)
  local list, err = rex.call("session.list")
  if not list then return nil, err end

  local now = os.time()
  local seen, unknown_sessions, found = {}, {}, {}

  for _, s in ipairs(list.sessions or {}) do
    local blocks = scall("session.list_blocks", s.session_id)
    if not blocks then
      unknown_sessions[s.session_id] = true
    else
      for _, b in ipairs(blocks.blocks or {}) do
        if b.creator_name == TERMINAL then
          local proc = bcall("process", s.session_id, b.block_id)
          if not proc then
            seen[b.block_id] = true
          else
            local agent, p = detect(proc, opts)
            if agent then
              seen[b.block_id] = true
              local e = state.entries[b.block_id]
              if not e or e.agent ~= agent or e.pid ~= p.pid then
                e = { agent = agent, pid = p.pid, first_seen = now }
                state.entries[b.block_id] = e
              end
              e.cwd = p.cwd or e.cwd
              e.env = nil
              for _, name in ipairs(opts.agents[agent].env or {}) do
                local value = process_env(p.pid, name)
                if value then
                  e.env = e.env or {}
                  e.env[name] = value
                end
              end
              e.rex_session_id = s.session_id
              e.rex_session_label = s.label
              e.window_id = b.window_id
              e.last_seen = now
              e.gone_at = nil
              if agent == "pi" then
                -- Its session ID is read later, once every pane is known: two
                -- panes in one directory share a candidate list.
                found[#found + 1] = { entry = e, pid = p.pid, cwd = e.cwd }
              elseif not e.agent_session_id then
                -- An agent added to the `agents` option has no file the
                -- tracker can read, so only its command line names a session.
                e.agent_session_id = id_from_args(p.pid)
              end
            end
          end
        end
      end
    end
  end

  -- Every pane is known now, so the sessions can be told apart.
  resolve_ids(found, opts)

  for block_id, e in pairs(state.entries) do
    if not seen[block_id] and not unknown_sessions[e.rex_session_id] then
      if not e.gone_at then
        e.gone_at = now
      elseif now - e.gone_at > opts.grace then
        state.entries[block_id] = nil
      end
    end
  end
  return true
end

---------------------------------------------------------------------------
-- Restore

-- The command typed to resume an entry's agent, or nil and a reason.
--
-- The state file is only data: it never holds a command. The command comes
-- from the `agents` option, and the state file fills in two values, both
-- checked here because they end up typed into a shell:
--
--   * the session ID, which must be a UUID and nothing else;
--   * the directory, which is typed only when the pane is somewhere else
--     (`pane_cwd`), must exist, and must have no control characters. A
--     terminal reads those as key presses, which quoting does not stop;
--   * the profile directories, held to the same rules. Only the variables
--     named in the `agents` option are ever set, whatever the file says.
local function plain_dir(path, must_own)
  if type(path) ~= "string" or path:sub(1, 1) ~= "/" or path:find("[%z\1-\31\127]") then
    return nil, "is not a plain absolute path"
  end
  -- Shells disagree about these inside single quotes: fish reads \' as a
  -- quote, PowerShell and Nushell don't read '\'' as one, and csh expands !.
  -- A directory name is chosen by whoever made the directory, a cloned
  -- repository included, so a path with any of them is never typed.
  if path:find("['\\!]") then
    return nil, "has a quote, backslash or exclamation mark in it"
  end
  if not exec({ "test", "-d", path }) then return nil, "no longer exists" end
  -- An agent runs the hooks and settings it finds in its profile directory,
  -- so one that someone else owns is never selected.
  if must_own and not exec({ "test", "-O", path }) then return nil, "is not owned by this user" end
  return true
end

local function resume_command(opts, e, pane_cwd)
  local spec = opts.agents[e.agent]
  if not spec or type(spec.resume) ~= "string" then return nil, "no resume command for " .. tostring(e.agent) end
  local id = e.agent_session_id
  if type(id) ~= "string" then return nil, "no agent session ID was captured" end
  if not id:match("^" .. UUID .. "$") then return nil, "the agent session ID is not a UUID" end
  local cmd = spec.resume:gsub("{id}", function() return id end)

  for i = #(spec.env or {}), 1, -1 do
    local name = spec.env[i]
    local value = type(e.env) == "table" and e.env[name] or nil
    if value ~= nil then
      if type(name) ~= "string" or not name:match("^[%a_][%w_]*$") then
        return nil, "bad variable name in the agents option"
      end
      local ok, why = plain_dir(value, true)
      if not ok then return nil, "the recorded " .. name .. " " .. why end
      cmd = name .. "=" .. shquote(value) .. " " .. cmd
    end
  end

  local cwd = e.cwd
  if cwd ~= nil and cwd ~= pane_cwd then
    local ok, why = plain_dir(cwd)
    if not ok then return nil, "the recorded directory " .. why end
    cmd = "cd -- " .. shquote(cwd) .. " && " .. cmd
  end
  return cmd
end

local uid
local function my_uid()
  if uid == nil then
    local out = exec({ "id", "-u" })
    uid = out and tonumber(out:match("%d+")) or false
  end
  return uid
end

-- The state file keys entries by block ID, but a server restart restores
-- sessions with new block IDs, so the recorded ID names a block that does
-- not exist. Resolve the entry's real pane through its session: a block at
-- the recorded working directory, or the session's only block. Returns the
-- live block ID and its process, or nil.
local function live_pane(e, block_id)
  local proc = bcall("process", e.rex_session_id, block_id)
  if proc then return block_id, proc end
  local blocks = scall("session.list_blocks", e.rex_session_id)
  if not blocks then return nil end
  local matched
  local only
  for _, b in ipairs(blocks.blocks or {}) do
    local p = bcall("process", e.rex_session_id, b.block_id)
    if p then
      if not only then only = { block_id = b.block_id, proc = p } end
      if e.cwd ~= nil and p.foreground and p.foreground.cwd == e.cwd then
        if matched then return nil end -- two panes match: refuse to guess
        matched = { block_id = b.block_id, proc = p }
      end
    end
  end
  if matched then return matched.block_id, matched.proc end
  if #(blocks.blocks or {}) == 1 and only then
    return only.block_id, only.proc
  end
  return nil
end

-- Resume the agent of each entry in its pane. `how.include_gone` also
-- resumes agents that had already left their pane. `how.dry` types nothing.
local function restore(state, opts, how)
  how = how or {}
  local results = {}

  local block_ids = {}
  for block_id in pairs(state.entries) do block_ids[#block_ids + 1] = block_id end
  table.sort(block_ids)

  for _, block_id in ipairs(block_ids) do
    local e = state.entries[block_id]
    local live_id, proc = live_pane(e, block_id)
    if live_id then block_id = live_id end
    local res = {
      block_id = block_id,
      agent = e.agent,
      agent_session_id = e.agent_session_id,
      rex_session = e.rex_session_label,
    }
    results[#results + 1] = res

    -- Rex restores a pane in the directory it was in, so the pane's own
    -- directory is used when it can be read, and no `cd` is typed.
    local pane_cwd = proc and proc.foreground and proc.foreground.cwd
    local cmd, why = resume_command(opts, e, pane_cwd)
    res.command = cmd
    if not cmd then
      res.result, res.reason = "skipped", why
    elseif e.gone_at and not how.include_gone then
      res.result, res.reason = "skipped", "the agent had exited before the restart"
    else
      if not proc then
        if not opts.reopen_missing then
          res.result, res.reason = "skipped", "the pane no longer exists"
        elseif how.dry then
          res.result = "would reopen"
        else
          local w, err = scall("session.new_window", e.rex_session_id, {
            layout = rex.layout.block{
              flavor = TERMINAL .. ".shell",
              options = { cwd = e.cwd, initial_input = cmd .. "\n" },
            },
          })
          if w then
            res.result, res.new_block_id = "reopened", w.block_ids and w.block_ids[1]
          else
            res.result, res.reason = "failed", tostring(err)
          end
        end
      elseif detect(proc, opts) then
        res.result, res.reason = "skipped", "an agent is already running in the pane"
      elseif not (proc.foreground and SHELLS[(proc.foreground.name or ""):gsub("^%-", "")]) then
        res.result = "skipped"
        res.reason = "the pane is busy with " .. tostring(proc.foreground and proc.foreground.name or "another program")
      elseif proc.foreground.user_id ~= nil and proc.foreground.user_id ~= my_uid() then
        res.result, res.reason = "skipped", "the pane's shell runs as another user"
      elseif how.dry then
        res.result = "would resume"
      else
        -- Ctrl-U first: it discards anything already typed at the prompt, so
        -- the command can't be appended to a half-written line.
        local w, err = bcall("write", e.rex_session_id, block_id, { data = "\21" .. cmd .. "\r" })
        if w then
          res.result = "resumed"
        else
          res.result, res.reason = "failed", tostring(err)
        end
      end
    end
  end
  return results
end

---------------------------------------------------------------------------
-- The tracker

local function pid_path(opts) return opts.state_dir .. "/daemon.pid" end

local function my_pid()
  local out = exec({ "sh", "-c", "echo $PPID" })
  return out and tonumber(out:match("%d+"))
end

-- The pid of the running tracker, or nil.
local function daemon_pid(opts)
  local text = read_file(pid_path(opts))
  local pid = text and tonumber(text:match("%d+"))
  if not pid then return nil end
  local args = process_args(pid)
  if args and args:find(" do ", 1, true) and args:find("daemon", 1, true) then return pid end
  return nil
end

-- The state directory is readable and writable by its owner only.
local function make_state_dir(opts)
  rex.exec({ "mkdir", "-p", opts.state_dir })
  rex.exec({ "chmod", "700", opts.state_dir })
end

local function daemon(opts)
  make_state_dir(opts)
  if not owned(opts.state_dir) then
    print("pi-resurrect: " .. opts.state_dir .. " is not owned by this user; not starting")
    return rex.stop(0)
  end
  log_path = opts.state_dir .. "/daemon.log"

  local me = my_pid()
  local other = daemon_pid(opts)
  if other and other ~= me then
    return rex.stop(0)
  end
  write_file(pid_path(opts), tostring(me) .. "\n")
  -- Two trackers started together both get this far. The last writer stays.
  rex.sleep(0.5)
  if tonumber((read_file(pid_path(opts)) or ""):match("%d+")) ~= me then
    return rex.stop(0)
  end

  local size = #(read_file(log_path) or "")
  if size > 1024 * 1024 then os.remove(log_path) end

  local state = load_state(opts)
  log("tracker started, pid %s, %d known agents", tostring(me), (function()
    local n = 0
    for _ in pairs(state.entries) do n = n + 1 end
    return n
  end)())

  -- Notice a server other than the one the state was written under, and
  -- resume what was running on the old one.
  local function check_instance()
    local status = rex.call("server.status")
    if not status or not status.instance_id then return false end
    if state.instance_id == status.instance_id then return true end

    local restarted = state.instance_id ~= nil
    state.instance_id = status.instance_id
    if not restarted then return true end

    log("server restarted (instance %s)", status.instance_id)
    if opts.auto_restore and next(state.entries) then
      rex.sleep(opts.restore_delay)
      local results = restore(state, opts, { include_gone = true })
      for _, r in ipairs(results) do
        log("restore %s %s in %s: %s%s", tostring(r.agent), tostring(r.agent_session_id),
          tostring(r.rex_session), r.result, r.reason and (" (" .. r.reason .. ")") or "")
      end
      write_file(opts.state_dir .. "/last-restore.json",
        rex.json.encode({ at = stamp(), results = results }, { indent = true }) or "{}")
    end
    -- Give every agent a fresh grace period to come back in.
    for _, e in pairs(state.entries) do e.gone_at = nil end
    return true
  end

  local retry
  local function tick()
    if not check_instance() then
      log("server did not answer, skipping scan")
      return
    end
    local ok, err = scan(state, opts)
    if not ok then
      log("scan failed: %s", tostring(err))
      return
    end
    local saved, serr = save_state(opts, state)
    if not saved then log("cannot write state: %s", tostring(serr)) end

    -- An agent takes a moment to record its session after it starts. Look
    -- again soon while a new agent has no session ID, so that a restart in
    -- its first seconds doesn't lose it.
    if retry then retry:cancel() end
    retry = nil
    local now = os.time()
    for _, e in pairs(state.entries) do
      if not e.gone_at and not e.agent_session_id and now - (e.first_seen or 0) < opts.id_retry_window then
        retry = rex.after(opts.id_retry_interval, tick)
        break
      end
    end
  end

  -- A process change means an agent may have started or exited. Scan at
  -- once, to record it, and again a little later, to catch a session the
  -- user picked by hand (`pi --session ID`, /resume) once the agent was up.
  local first, second
  local function soon()
    if first then first:cancel() end
    if second then second:cancel() end
    first = rex.after(opts.event_delay, tick)
    second = rex.after(opts.followup_delay, tick)
  end

  rex.on("terminal.process_changed", soon)
  rex.on("block_created", soon)
  rex.on("block_closed", soon)
  rex.on("session_destroyed", soon)
  rex.every(opts.scan_interval, tick)
  tick()
end

---------------------------------------------------------------------------
-- Reports

local function summary(opts, state)
  local now = os.time()
  local agents = {}
  for block_id, e in pairs(state.entries) do
    agents[#agents + 1] = {
      agent = e.agent,
      agent_session_id = e.agent_session_id,
      cwd = e.cwd,
      rex_session = e.rex_session_label,
      block_id = block_id,
      state = e.gone_at and ("gone " .. (now - e.gone_at) .. "s ago") or "running",
      resume = (resume_command(opts, e, e.cwd)),
    }
  end
  table.sort(agents, function(a, b) return a.block_id < b.block_id end)
  return {
    tracker_pid = daemon_pid(opts),
    state_file = state_path(opts),
    last_scan = state.heartbeat and stamp(state.heartbeat) or nil,
    agents = agents,
  }
end

---------------------------------------------------------------------------
-- init.lua side

local function script_path()
  local src = debug.getinfo(1, "S").source or ""
  return (src:gsub("^@", ""))
end

-- The server's own binary and command line, as seen from a command it runs.
local function parent_process()
  -- On Linux, a binary replaced by an upgrade while the server runs reads as
  -- "/usr/bin/rex (deleted)". The new binary is at the same path, so drop
  -- the suffix, and fall back to PATH when nothing executable is there.
  local bin = exec({ "sh", "-c", [[
b=$(readlink "/proc/$PPID/exe" 2>/dev/null) || b=$(ps -o comm= -p "$PPID")
b=${b% (deleted)}
[ -x "$b" ] || b=$(command -v rex 2>/dev/null) || b=
printf '%s' "$b"
]] })
  local args = exec({ "sh", "-c", "ps -o args= -p $PPID" })
  return bin and bin:gsub("%s+$", ""), args or ""
end

local function run_cli(opts, args)
  local bin = opts.rex_bin or parent_process()
  -- Arguments go in on standard input, so they never show in `ps`.
  local r, err = rex.exec({
    bin, "do", script_path(), "--args", "-",
  }, { timeout = 60, stdin = rex.json.encode(args) })
  if not r then return nil, err end
  if not r.ok then return nil, (r.stderr ~= "" and r.stderr or r.stdout) end
  return rex.json.decode(r.stdout) or r.stdout
end

local function start_daemon(opts)
  local bin, args = parent_process()
  -- Only the server starts the tracker. `rex config check` loads this file
  -- too, and must not leave a process behind.
  if not args:find(" server", 1, true) then return end
  bin = opts.rex_bin or bin
  if not bin or bin == "" then
    rex.log("warn", "pi-resurrect: cannot find the rex binary; set rex_bin")
    return
  end

  make_state_dir(opts)
  -- The options are read from a file in the private state directory rather
  -- than passed on the command line, where any user could read them.
  local args_file = opts.state_dir .. "/daemon-args.json"
  local wrote, werr = write_file(args_file, rex.json.encode({ cmd = "daemon", opts = opts }))
  if not wrote then
    rex.log("warn", "pi-resurrect: cannot write " .. args_file .. ": " .. tostring(werr))
    return
  end
  -- The server may not be listening yet while it loads its configuration,
  -- so keep trying for a minute. A tracker that ran and exited cleanly, as
  -- a duplicate does, ends the loop.
  local loop = [[
n=0
while [ "$n" -lt 20 ]; do
  "$1" --autostart=false do "$2" --all --args "$3" >>"$4" 2>&1 && exit 0
  n=$((n+1))
  sleep 3
done
]]
  local h, err = rex.exec.start({
    "sh", "-c", loop, "pi-resurrect",
    bin, script_path(),
    "@" .. args_file,
    opts.state_dir .. "/daemon.out",
  }, { detach = true, stdout = false, stderr = false })
  if not h then rex.log("warn", "pi-resurrect: cannot start the tracker: " .. tostring(err)) end
end

function M.setup(user_opts)
  local opts = merge(defaults, user_opts)
  M.opts = opts
  -- Loaded by a script rather than the server: nothing to set up.
  if rex.args ~= nil then return M end

  rex.action{
    name = "pi_resurrect_status",
    title = "Pi Resurrect: Show Tracked Agents",
    category = "Pi Resurrect",
    description = "List the pi agents pi-resurrect is tracking and how each would be resumed.",
    run = function() return summary(opts, load_state(opts)) end,
  }

  rex.action{
    name = "pi_resurrect_restore",
    title = "Pi Resurrect: Resume Tracked Agents",
    category = "Pi Resurrect",
    description = "Type each tracked pi agent's resume command into its pane, where the pane is at a shell prompt.",
    args = { all = "boolean?", dry = "boolean?" },
    run = function(_, args)
      args = args or {}
      -- A gone agent is exactly what a restart leaves at a shell prompt, so
      -- it is resumed unless the caller rules it out.
      return run_cli(opts, { cmd = "restore", all = args.all ~= false, dry = args.dry, opts = opts })
    end,
  }

  rex.action{
    name = "pi_resurrect_restart",
    title = "Pi Resurrect: Restart Tracker",
    category = "Pi Resurrect",
    description = "Stop the background tracker and start it again with the current configuration.",
    run = function()
      run_cli(opts, { cmd = "stop", opts = opts })
      start_daemon(opts)
      return "restarted"
    end,
  }

  start_daemon(opts)
  return M
end

---------------------------------------------------------------------------
-- Script side: rex do pi-resurrect.lua [cmd=status|scan|restore|stop|selftest|daemon]

if MODNAME == nil and rex.args ~= nil then
  local a = rex.args
  local opts = merge(defaults, type(a.opts) == "table" and a.opts or {})
  local cmd = a.cmd or "status"

  if cmd == "daemon" then
    daemon(opts)
    return
  elseif cmd == "status" then
    return summary(opts, load_state(opts))
  elseif cmd == "scan" then
    -- Look now. Nothing is written unless write=true, so a running tracker
    -- keeps sole charge of the state file.
    local state = a.write and load_state(opts) or { entries = {} }
    local ok, err = scan(state, opts)
    if not ok then error(tostring(err), 0) end
    if a.write then
      make_state_dir(opts)
      save_state(opts, state)
    end
    return summary(opts, state)
  elseif cmd == "restore" then
    return restore(load_state(opts), opts, { include_gone = a.all ~= false, dry = a.dry })
  elseif cmd == "stop" then
    local pid = daemon_pid(opts)
    if not pid then return "the tracker is not running" end
    rex.exec({ "kill", tostring(pid) })
    return "stopped tracker " .. pid
  elseif cmd == "selftest" then
    -- The pane-to-session assignment, on made-up panes and files: each pane
    -- must land on a different session, and on the one it really holds.
    local function pane(held, first_seen)
      return { entry = { agent_session_id = held, first_seen = first_seen or 0 } }
    end
    local function files(...)
      local list = {}
      for _, spec in ipairs({ ... }) do
        list[#list + 1] = { id = spec[1], fresh = spec[2] }
      end
      return list
    end
    local failed, checks = {}, 0
    local function joined(t)
      local s = ""
      for _, v in ipairs(t) do s = s .. "," .. tostring(v) end
      return s
    end
    local function check(what, items, candidates, want)
      checks = checks + 1
      assign(items, candidates)
      local got = {}
      for _, it in ipairs(items) do got[#got + 1] = it.entry.agent_session_id end
      table.sort(got)
      table.sort(want)
      if joined(got) ~= joined(want) then
        failed[#failed + 1] = what .. ": got " .. joined(got) .. ", want " .. joined(want)
      end
    end
    check("an idle pane keeps its session",
      { pane("x") }, files({ "x", false }), { "x" })
    check("a new pane takes the file being written",
      { pane(nil) }, files({ "y", true }, { "x", false }), { "y" })
    check("/new gives up the file that stopped being written",
      { pane("x") }, files({ "z", true }, { "x", false }), { "z" })
    check("two idle panes keep their own sessions",
      { pane("x", 2), pane("y", 1) }, files({ "x", false }, { "y", false }), { "x", "y" })
    check("two panes in one directory never share a session",
      { pane(nil, 2), pane(nil, 1) }, files({ "b", true }, { "a", true }), { "a", "b" })
    check("a second pane does not take the first one's session",
      { pane("x", 2), pane(nil, 1) }, files({ "z", true }, { "x", false }), { "x", "z" })
    check("a pane with no file to read gets no ID",
      { pane(nil) }, files(), {})
    if #failed > 0 then error(table.concat(failed, "; "), 0) end
    return checks .. " checks passed"
  else
    error("unknown cmd: " .. tostring(cmd) .. " (status, scan, restore, stop, selftest, daemon)", 0)
  end
end

return M
