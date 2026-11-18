{
  config,
  lib,
  pkgs,
  isDarwin,
  ...
}:
let
  cfg = config.modules.rex;

  # Rex's whole config surface is one init.lua plus what it loads.  The project
  # picker -- Lua in Rex's engine plus a bash helper driving fd -- lives next to
  # this module and deploys as plain files, the way the source dotfiles stow it.
  # Copied from pkgs/rex-project-picker in
  # https://github.com/pperanich/dotfiles at 29ea77f4292ea8f0bb58b5a23bddeaefa4803536
  # (MIT); picker.lua carries the local changes it needed here -- a "/bin" tool
  # prefix, and Rex's palette (rex.ui.pick) instead of upstream's fzf popup.
  #
  # Switching sessions needs no picker of our own: the app ships one, so that key
  # below only binds the client's session.switch action.
  pickerSrc = ./rex-project-picker;

  # builtins.toJSON gives Lua-compatible strings and numbers, but its objects use
  # ":" where Lua wants "=", so the root entries are laid out by hand.
  rootsLua = lib.concatMapStringsSep ", " (
    root: "{ path = ${builtins.toJSON root.path}, depth = ${toString root.depth} }"
  ) cfg.projectRoots;

  initLua = ''
    local home = assert(os.getenv("HOME"))
    local config_home = os.getenv("XDG_CONFIG_HOME") or (home .. "/.config")
    local picker = dofile(config_home .. "/rex/rex-project-picker/picker.lua")

    picker.setup({
      roots = { ${rootsLua} },
      codeRoot = ${builtins.toJSON cfg.codeRoot},
    })

    -- The ctrl+n>e binding below hands a pane to Emacs.  It needs only server
    -- calls plus emacsclient, so -- unlike the picker's session.select -- it
    -- works with Remote Control off.  The Emacs half is
    -- `rex-gt-open-pane-directory' in modules/editor/emacs/packages/rex-gt.el,
    -- which opens the directory it is given in Dired.
    local function pane_directory(session_id, block_id)
      local process = rex.call("block.method", {
        session_id = session_id,
        block_id = block_id,
        method = "process",
      })
      local cwd = process and process.foreground and process.foreground.cwd
      if cwd then
        return cwd
      end
      -- Nothing readable to inspect (a root shell, say), so fall back to the
      -- directory the shell reported with OSC 7.  A path on another host is
      -- not ours to open here.
      local pwd = rex.call("block.method", {
        session_id = session_id,
        block_id = block_id,
        method = "pwd",
      })
      local path = pwd and pwd.pwd
      if type(path) ~= "string" then
        return nil
      end
      -- `and` keeps only the first return value, so match outside the guard.
      local host, encoded = path:match("^file://([^/]*)(/.*)$")
      if not encoded or (host ~= "" and host ~= "localhost") then
        return nil
      end
      return (encoded:gsub("%%(%x%x)", function(hex) return string.char(tonumber(hex, 16)) end))
    end

    rex.action({
      name = "rex_open_in_emacs",
      title = "Open Pane in Emacs",
      run = function(ctx)
        assert(ctx.session_id, "Open this from a Rex session")
        local view = rex.call("session.view", {session_id = ctx.session_id})
        local where, block_id
        for _, window in ipairs(view.windows or {}) do
          if window.window_id == view.active_window_id then
            where = (view.label or ctx.session_id) .. "/" .. (window.label or "")
            block_id = window.focused_block_id
          end
        end
        assert(block_id, "This session has no focused pane")
        local directory = pane_directory(ctx.session_id, block_id)
        assert(directory, "This pane reports no working directory")
        local reply = rex.exec({ "emacsclient", "-n", "-e",
          "(rex-gt-open-pane-directory " .. rex.json.encode(directory)
            .. " " .. rex.json.encode(where) .. ")" }, { timeout = 10 })
        if not reply.ok then
          error("Emacs did not answer: " .. (reply.stderr or "emacsclient failed"), 0)
        end
      end,
    })

    rex.bind(${builtins.toJSON cfg.pickerKey}, "rex_project_picker")
    rex.bind(${builtins.toJSON cfg.sessionPickerKey}, "session.switch")
    rex.bind(${builtins.toJSON cfg.splitDownKey}, "pane.split.down")
    rex.bind(${builtins.toJSON cfg.splitRightKey}, "pane.split.right")
    rex.bind(${builtins.toJSON cfg.toggleBarKey}, "client.sidebar.toggle")
    rex.bind(${builtins.toJSON cfg.openInEmacsKey}, "rex_open_in_emacs")
  '';
in
with lib;
{
  options.modules.rex = {
    enable = mkEnableOption "Rex terminal multiplexer";

    projectRoots = mkOption {
      type =
        with types;
        listOf (submodule {
          options = {
            path = mkOption {
              type = types.str;
              description = "Absolute directory to search for git repositories.";
            };
            depth = mkOption {
              type = types.ints.between 0 32;
              default = 2;
              description = "Levels below path to search; 0 checks only path.";
            };
          };
        });
      default = [
        {
          path = "${config.my.homeDirectory}/org";
          depth = 1;
        }
        {
          path = "${config.my.homeDirectory}/workspaces";
          # 3 for the workspaces/<host>/<owner>/<repo> layout `h` expects (see
          # github.com/zimbatm/h).  A repo's linked worktrees sit beside it in the
          # same owner dir, so they need no extra level.  Clones nested inside a
          # repo (the events-idl vendored copies) fall past this depth on purpose.
          depth = 3;
        }
      ];
      description = "Roots the project picker walks.";
    };

    codeRoot = mkOption {
      type = types.str;
      default = config.modules.h.codeRoot;
      description = ''
        Root `h' (github.com/zimbatm/h) resolves against when the picker's palette
        is handed a repo name, an owner/repo or a git URL, cloning it first when it
        is missing.  Shares modules.h.codeRoot so the two layouts cannot drift
        apart.
      '';
    };

    pickerKey = mkOption {
      type = types.str;
      default = "ctrl+n>p";
      description = "Shortcut that opens the project picker.";
    };

    sessionPickerKey = mkOption {
      type = types.str;
      default = "ctrl+n>s";
      description = "Shortcut that opens Rex's own session switcher.";
    };

    splitDownKey = mkOption {
      type = types.str;
      default = "ctrl+n>v";
      description = "Shortcut that splits the focused pane downward.";
    };

    splitRightKey = mkOption {
      type = types.str;
      default = "ctrl+n>h";
      description = "Shortcut that splits the focused pane to the right.";
    };

    toggleBarKey = mkOption {
      type = types.str;
      default = "ctrl+n>t>s";
      description = "Shortcut that hides or shows the tab bar.";
    };

    openInEmacsKey = mkOption {
      type = types.str;
      default = "ctrl+n>e";
      description = ''
        Shortcut that opens the focused pane's working directory in Emacs.
        Needs `modules.editors.emacs.enable' on this host, since the action
        calls emacsclient and the Elisp it evaluates lives in that module.
      '';
    };
  };

  config = mkIf cfg.enable (
    {
      home-manager.users.${config.my.username} =
        hm@{ ... }:
        {
          # rex plus the picker's own dependencies: picker.lua resolves each tool
          # under the user profile (the macOS system prefixes only cover bash and
          # pwd), so they have to be linked there rather than named in init.lua.
          # fzf needs no entry -- the palette replaced it -- and h comes from
          # modules.h.
          home.packages = [
            pkgs.fd
            pkgs.rex
          ];

          xdg.configFile = {
            "rex/init.lua".text = initLua;

            # picker.lua resolves its own directory and runs the helper from it, so
            # the two must stay siblings.  bash takes the helper as an argument, so
            # it needs no exec bit.  picker-selfcheck.lua is a dev-only check, so it
            # is deliberately not listed here.
            "rex/rex-project-picker/picker.lua".source = "${pickerSrc}/picker.lua";
            "rex/rex-project-picker/rex-project-picker".source = "${pickerSrc}/rex-project-picker";

            # The Ghostty Catppuccin Mocha palette.  Rex stores its chosen theme in
            # the macOS app's preferences, so deployment alone changes nothing:
            # import it once, then pick it in the theme picker
            #   rex --autostart=false -C CLIENT do client.theme.import \
            #     path="$HOME/.config/rex/themes/Catppuccin Mocha"
            "rex/themes/Catppuccin Mocha".text = ''
              palette = 0=#45475a
              palette = 1=#f38ba8
              palette = 2=#a6e3a1
              palette = 3=#f9e2af
              palette = 4=#89b4fa
              palette = 5=#f5c2e7
              palette = 6=#94e2d5
              palette = 7=#bac2de
              palette = 8=#585b70
              palette = 9=#f7aec2
              palette = 10=#c2ecbf
              palette = 11=#fcd682
              palette = 12=#aeccfc
              palette = 13=#f398da
              palette = 14=#b1eae1
              palette = 15=#a6adc8
              background = #1e1e2e
              foreground = #cdd6f4
              cursor-color = #f5e0dc
              cursor-text = #1e1e2e
              selection-background = #f5e0dc
              selection-foreground = #1e1e2e
            '';

            # The upstream Rex SSH guard, kept here as a file rather than installed
            # by `rex terminfo setup` so it stays in the generation.  Nothing reads
            # it yet: home-manager cannot take over the hand-written ~/.ssh/config,
            # so add `Include ~/.config/rex/ssh_config` there by hand.
            "rex/ssh_config".text = ''
              # Remote programs usually know xterm-256color, but not xterm-rex.
              # List exact hostnames that do have xterm-rex in
              # ~/.config/rex/terminfo-hosts to opt them out of this fallback.
              Match exec "/bin/sh -c '[ x$TERM = xxterm-rex ] && ! grep -qxF -- %h ~/.config/rex/terminfo-hosts 2>/dev/null'"
                SetEnv TERM=xterm-256color

              # An Include shares parsing context with its parent; do not leak this Match.
              Match all
            '';
          };

          # The agent skill and its hooks are not generation files -- rex writes
          # ~/.agents/skills/rex and each agent's own settings file -- so they come
          # from the CLI instead of xdg.configFile.  Setup is idempotent and reads
          # its guide out of the binary it is called from, so this run is also what
          # refreshes the skill when rex is upgraded.  --autostart keeps a switch
          # from starting the Rex server.
          home.activation.rexAgentSkill = hm.config.lib.dag.entryAfter [ "writeBoundary" ] ''
            if [ -n "''${DRY_RUN:-}" ]; then
              exit 0
            fi
            # rex finds an agent by its config directory or its program on $PATH,
            # and the activation PATH is minimal, so spell out the user profile.
            PATH=${lib.escapeShellArg "/etc/profiles/per-user/${config.my.username}/bin:/run/current-system/sw/bin:/usr/local/bin:/usr/bin:/bin"} \
              ${lib.getExe' pkgs.rex "rex"} --autostart=false skill setup \
              || echo "warning: 'rex skill setup' failed; run it by hand" >&2
          '';
        };
    }
    # optionalAttrs, not mkIf: this module is in myModules.common, and the
    # `system.defaults` option name alone fails evaluation on NixOS hosts.
    // optionalAttrs isDarwin {
      # Rex keeps these in memory and flushes the domain when it quits, so a
      # rebuild with the app open has its write overwritten on quit: quit first,
      # then rebuild, then relaunch.  Sessions live in the server, not the app, so
      # quitting costs no running programs.
      #   osascript -e 'quit app "Rex"' && sudo darwin-rebuild switch --flake '.#mist' && open -a 'Rex'
      system.defaults.CustomUserPreferences."com.superlogical.rex" = {
        # Remote Control is the gate on every client action, including the
        # session.select the project picker queues to focus what it opened -- with
        # it off the picker creates the session and then fails.  Anything that can
        # reach the local server can then drive the app, which is what the CLI
        # automation is for, but it is a wider door than the font settings above.
        "rex.client.remoteControl" = true;
        # Tabs down the left edge instead of across the top.  Shift-cmd-s only
        # flips tabStyle, so on its own it lands on the right side of the screen.
        "Workspace.tabStyle" = "vertical";
        "Workspace.sidebarPosition" = "left";
        # Must name an installed face exactly; the same value Alacritty uses.
        "Workspace.terminalFontName" = config.my.monoFont;
        # A Nix float -- nix-darwin throws on the string "16.0".
        "Workspace.terminalFontPointSize" = 18.0;
        "Workspace.terminalAllowsLigatures" = true;
        "Workspace.terminalCopyOnSelect" = true;
      };
    }
  );
}
