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
  # picker -- Lua in Rex's engine plus a bash helper driving fd and fzf -- lives
  # next to this module and deploys as plain files, the way the source dotfiles
  # stow it.  Copied from pkgs/rex-project-picker in
  # https://github.com/pperanich/dotfiles at 29ea77f4292ea8f0bb58b5a23bddeaefa4803536
  # (MIT); picker.lua carries the one local change it needed here.
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
    })

    rex.bind(${builtins.toJSON cfg.pickerKey}, "rex_project_picker")
    rex.bind(${builtins.toJSON cfg.sessionPickerKey}, "session.switch")
    rex.bind(${builtins.toJSON cfg.splitDownKey}, "pane.split.down")
    rex.bind(${builtins.toJSON cfg.splitRightKey}, "pane.split.right")
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
          # 2 so the picker also finds workmux worktrees under a repo.
          depth = 2;
        }
      ];
      description = "Roots the project picker walks.";
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
  };

  config = mkIf cfg.enable (
    {
      home-manager.users.${config.my.username} =
        hm@{ ... }:
        {
          # rex plus the picker's own dependencies: picker.lua resolves each tool
          # under the user profile (the macOS system prefixes only cover bash and
          # pwd), so they have to be linked there rather than named in init.lua.
          home.packages = [
            pkgs.fd
            pkgs.fzf
            pkgs.rex
          ];

          xdg.configFile = {
            "rex/init.lua".text = initLua;

            # picker.lua resolves its own directory and runs the helper from it, so
            # the two must stay siblings.  bash takes the helper as an argument, so
            # it needs no exec bit.
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
      # Rex reads these at launch and rewrites the plist when it quits, so quit
      # Rex before darwin-rebuild or the write is lost.
      system.defaults.CustomUserPreferences."com.superlogical.rex" = {
        # Remote Control is the gate on every client action, including the
        # session.select the project picker queues to focus what it opened -- with
        # it off the picker creates the session and then fails.  Anything that can
        # reach the local server can then drive the app, which is what the CLI
        # automation is for, but it is a wider door than the font settings above.
        "rex.client.remoteControl" = true;
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
