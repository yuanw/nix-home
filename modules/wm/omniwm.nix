# modules/wm/omniwm.nix
#
# OmniWM (https://github.com/OmniNull/OmniWM) in place of yabai: it tiles,
# draws its own workspace bar and focused-window border, and takes commands over
# IPC, so skhd calls omniwmctl where modules/wm/skhdrc used to call yabai.
#
# Needs macOS 26+ on Apple Silicon and Mission Control's "Displays have separate
# Spaces" on (the macOS default); OmniWM pauses tiling while it is off. Turning
# this module off and modules.wm.yabai back on reverts the whole stack, since
# sketchybar and jankyborders are configured by modules/wm/yabai.nix.
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.wm.omniwm;
  username = config.my.username;
  # Absolute path: the skhd agent runs with launchd's PATH, not the user's.
  omni = args: "${pkgs.omniwm}/bin/omniwmctl command ${args}";
  launcher = import ./skhd-launcher.nix { inherit config pkgs; };

  # The template doubles as the record of how many workspaces there are, so the
  # skhd chords below cannot drift from it the way a hardcoded count did when a
  # seventh workspace appeared in the GUI.
  wmSettings = builtins.fromTOML (builtins.readFile ./omniwm/settings.toml);

  # The template pins workspaces to displays by displayUUID, which is only
  # valid for the machine whose displays it was generated on. A host that sets
  # workspaceMonitors gets its own per-workspace assignments layered on top of
  # the template's workspaces; unset, the template is used as-is.
  monitorAssignment =
    a:
    if a.type == "main" then
      { type = "main"; }
    else
      {
        type = "specificDisplay";
        output = {
          displayUUID = a.uuid;
          name = a.name;
        };
      };

  finalSettings =
    if
      cfg.workspaceMonitors == null
      || builtins.length cfg.workspaceMonitors != builtins.length wmSettings.workspaces
    then
      wmSettings
    else
      wmSettings
      // {
        workspaces = map (
          i:
          builtins.elemAt wmSettings.workspaces i
          // {
            monitorAssignment = monitorAssignment (builtins.elemAt cfg.workspaceMonitors i);
          }
        ) (builtins.genList (i: i) (builtins.length wmSettings.workspaces));
      };

  # The workspace trio modules/wm/skhdrc had: focus, send, send and follow, for
  # each workspace the settings template declares. omniwmctl takes the 1-based
  # position, not the workspace name, so the count is all this reads from it.
  workspaceKeys = lib.concatStringsSep "\n" (
    builtins.map (n: ''
      cmd - ${n} : ${omni "switch-workspace ${n}"}
      shift + cmd - ${n} : ${omni "move-to-workspace ${n}"}
      ctrl + cmd - ${n} : ${omni "move-to-workspace ${n}"} && ${omni "switch-workspace ${n}"}
    '') (builtins.genList (i: toString (i + 1)) (builtins.length wmSettings.workspaces))
  );

  wmConfig = ''
    # next / previous window in the column or group (was yabai-next-window)
    shift + ctrl + alt - j : ${omni "focus-window down-or-top"}
    shift + ctrl + alt - k : ${omni "focus-window up-or-bottom"}

    # move the whole tile or column (yabai window --swap)
    shift + alt - h : ${omni "move-column left"}
    shift + alt - j : ${omni "move-column down"}
    shift + alt - k : ${omni "move-column up"}
    shift + alt - l : ${omni "move-column right"}

    # move the focused window (yabai window --warp)
    shift + cmd - h : ${omni "move left"}
    shift + cmd - j : ${omni "move down"}
    shift + cmd - k : ${omni "move up"}
    shift + cmd - l : ${omni "move right"}

    # sizing
    shift + alt - 0 : ${omni "balance-sizes"}
    shift + alt - up : ${omni "toggle-fullscreen"}
    shift + alt - left : ${omni "cycle-size backward"}
    shift + alt - right : ${omni "cycle-size forward"}
    # yabai window --resize: grow from the edge the key names
    shift + alt - a : ${omni "resize horizontal grow"}
    shift + alt - d : ${omni "resize horizontal grow"}
    shift + alt - w : ${omni "resize vertical grow"}
    shift + alt - s : ${omni "resize vertical grow"}
    shift + cmd - a : ${omni "resize horizontal shrink"}
    shift + cmd - d : ${omni "resize horizontal shrink"}
    shift + cmd - w : ${omni "resize vertical shrink"}
    shift + cmd - s : ${omni "resize vertical shrink"}
    shift + alt - e : ${omni "toggle-workspace-layout"}

    # workspaces
    cmd + alt - x : ${omni "focus previous"}
    cmd + alt - z : ${omni "switch-workspace prev"}
    cmd + alt - c : ${omni "switch-workspace next"}
    ${workspaceKeys}

    # monitors
    ctrl + alt - x : ${omni "focus-monitor last"}
    ctrl + alt - z : ${omni "focus-monitor prev"}
    ctrl + alt - c : ${omni "focus-monitor next"}
    cmd + alt + shift - h : ${omni "move-to-monitor left"}
    cmd + alt + shift - j : ${omni "move-to-monitor down"}
    cmd + alt + shift - k : ${omni "move-to-monitor up"}
    cmd + alt + shift - l : ${omni "move-to-monitor right"}

    # float / tile the focused window (yabai window --toggle float)
    cmd + alt + shift - t : ${omni "toggle-focused-window-floating"}
    # OmniWM has no space to destroy, so the palette and Overview take the keys
    # that have no native binding worth keeping here
    cmd + alt + shift - p : ${omni "open-command-palette"}
    cmd + alt + shift - o : ${omni "toggle-overview"}

    # reload skhd and restart the WM (the agent is org.nixos.omniwm)
    shift + ctrl + alt - r: ${pkgs.skhd}/bin/skhd -r && \
                            launchctl kickstart -k "gui/$(id -u)/org.nixos.omniwm" && \
                            osascript -e 'display notification "reload skhd and restart OmniWM"'
  '';
in
{
  options.modules.wm.omniwm = {
    enable = lib.mkEnableOption "OmniWM";

    githubInbox = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Refresh the GitHub bar widget's inbox by running gh on a launchd timer.";
    };

    githubInboxInterval = lib.mkOption {
      type = lib.types.port;
      default = 120;
      description = "Seconds between GitHub inbox refreshes.";
    };

    workspaceMonitors = lib.mkOption {
      type = lib.types.nullOr (
        lib.types.listOf (
          lib.types.submodule {
            options = {
              type = lib.mkOption {
                type = lib.types.enum [
                  "main"
                  "specificDisplay"
                ];
                default = "main";
              };
              name = lib.mkOption {
                type = lib.types.str;
                default = "";
                description = "Display name as OmniWM reports it, e.g. \"BenQ RD280U\".";
              };
              uuid = lib.mkOption {
                type = lib.types.str;
                default = "";
                description = "Display UUID from `omniwmctl query displays --fields uuid`.";
              };
            };
          }
        )
      );
      default = null;
      description = "Per-workspace monitor assignment, in settings.toml template order. Null keeps the template's own displayUUID assignments (valid only for the host it was generated on).";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !config.modules.wm.yabai.enable;
        message = "modules.wm.omniwm and modules.wm.yabai cannot both be enabled: two window managers cannot tile the same windows.";
      }
      {
        assertion =
          cfg.workspaceMonitors == null
          || builtins.length cfg.workspaceMonitors == builtins.length wmSettings.workspaces;
        message = "modules.wm.omniwm.workspaceMonitors must list one entry per workspace in modules/wm/omniwm/settings.toml (${toString (builtins.length wmSettings.workspaces)}).";
      }
      {
        assertion =
          cfg.workspaceMonitors == null
          || lib.all (a: a.type == "main" || (a.name != "" && a.uuid != "")) cfg.workspaceMonitors;
        message = "modules.wm.omniwm.workspaceMonitors entries with type \"specificDisplay\" need name and uuid (see `omniwmctl query displays --fields name,uuid`).";
      }
    ];

    home-manager.users.${username} = {
      home.packages = launcher.packages ++ [ pkgs.omniwm ];

      # The GitHub inbox button, ported from modules/wm/sketchybar/plugins/github.sh
      # (see modules/wm/omniwm/widgets/github). It reads what the
      # omniwm-github-inbox agent publishes, so there is no token to give OmniWM
      # and no host to allow -- gh still does the auth, as it did for sketchybar.
      # The top-level shell/widgets folder serves every profile; add it to a lane
      # in Settings > Styles & Widgets > Bar.
      xdg.configFile."omniwm/shell/widgets/github".source = ./omniwm/widgets/github;

      # OmniWM owns this file: its Settings window rewrites settings.toml, so the
      # path has to be a regular file OmniWM can write, not a symlink back to the
      # store that the next switch clobbers (force re-linked the template and the
      # GUI edits made since were simply gone). mergetools merges the document
      # below into whatever is there instead: the keys we declare win, keys only
      # OmniWM writes (new schema keys after an upgrade, state it adds) survive,
      # and a symlinked or read-only target is materialised to a writable file.
      # Backups land next to the target as settings.toml.backup.<ts> and
      # ~/.config/omniwm/.settings.toml.nix-managed shows what Nix forces.
      #
      # The template stays the whole document -- see the comment at its top.
      mergetools.omniwm-settings = {
        target = "${config.my.homeDirectory}/.config/omniwm/settings.toml";
        format = "toml";
        force = true;
        settings = finalSettings;
      };
    };

    services.skhd = {
      enable = true;
      skhdConfig = ''
        # launcher chords shared with modules/wm/yabai.nix
        ${launcher.config}

        # layout chords
        ${wmConfig}
      '';
    };

    launchd.user.agents.skhd.serviceConfig = {
      StandardOutPath = "/tmp/skhd.log";
      StandardErrorPath = "/tmp/skhd.log";
    };

    # skhd never re-creates its CGEventTap after sleep/wake and never checks the
    # result of CGEventTapCreate, so a tap lost across sleep leaves it up,
    # silent and deaf. Same reason as the copy in modules/wm/yabai.nix.
    launchd.user.agents.skhd-watchdog = {
      serviceConfig = {
        ProgramArguments = [
          "/bin/sh"
          "-c"
          "/bin/launchctl kickstart -k gui/$(id -u '${username}')/org.nixos.skhd"
        ];
        RunAtLoad = true;
        # in secs
        StartInterval = 900;
      };
    };

    # The GitHub widget renders what this agent publishes; a Luau widget cannot
    # start a program, so gh runs here instead -- the same pair of programs the
    # sketchybar plugin drove, github-notifications-filter.py shared with it, so
    # the dismissed-thread log comes along too. launchd gives no PATH, and the
    # skhd lesson above applies: name the programs it needs.
    launchd.user.agents.omniwm-github-inbox = lib.mkIf cfg.githubInbox {
      serviceConfig = {
        ProgramArguments = [
          "/bin/sh"
          "${./omniwm/github-inbox.sh}"
        ];
        EnvironmentVariables = {
          PATH = lib.makeBinPath [
            pkgs.gh
            pkgs.coreutils
            pkgs.git
            pkgs.jq
            pkgs.python3
            pkgs.omniwm
          ];
          FILTER = "${./sketchybar/plugins/github-notifications-filter.py}";
          GITHUB_NOTIFICATIONS_LOG_DIR = "${config.my.homeDirectory}/.local/state/omniwm";
        };
        RunAtLoad = true;
        StartInterval = cfg.githubInboxInterval;
        StandardOutPath = "/tmp/omniwm-github-inbox.log";
        StandardErrorPath = "/tmp/omniwm-github-inbox.log";
      };
    };

    launchd.user.agents.omniwm = {
      serviceConfig = {
        Program = "${pkgs.omniwm}/Applications/OmniWM.app/Contents/MacOS/OmniWM";
        RunAtLoad = true;
        KeepAlive = true;
        StandardOutPath = "/tmp/omniwm.log";
        StandardErrorPath = "/tmp/omniwm.err.log";
      };
    };

    system.activationScripts.postActivation.text = lib.mkAfter ''
      # OmniWM needs Accessibility and Input Monitoring before it will manage
      # windows; without them it stays up and quietly manages nothing. Grant
      # them by bundle id, the same way modules/wm/yabai.nix does for yabai,
      # then restart the agent so it re-reads its permissions. First launch
      # still prompts (and Screen Recording for Overview thumbnails stays a
      # manual grant): a TCC row for a notarized app without its code
      # requirement is not always honoured.
      TCCUTIL="${pkgs.tccutil-manage}/bin/tccutil-manage"
      echo "Granting Accessibility and Input Monitoring to OmniWM..."
      "$TCCUTIL" --insert com.barut.OmniWM || true
      "$TCCUTIL" --service kTCCServiceListenEvent --insert com.barut.OmniWM || true
      launchctl kickstart -k "gui/$(id -u '${username}')/org.nixos.omniwm" || true
    '';
  };
}
