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

  # The workspace trio modules/wm/skhdrc had: focus, send, send and follow.
  workspaceKeys = lib.concatStringsSep "\n" (
    builtins.map (n: ''
      cmd - ${n} : ${omni "switch-workspace ${n}"}
      shift + cmd - ${n} : ${omni "move-to-workspace ${n}"}
      ctrl + cmd - ${n} : ${omni "move-to-workspace ${n}"} && ${omni "switch-workspace ${n}"}
    '') (map toString (lib.range 1 9))
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
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !config.modules.wm.yabai.enable;
        message = "modules.wm.omniwm and modules.wm.yabai cannot both be enabled: two window managers cannot tile the same windows.";
      }
    ];

    home-manager.users.${username} = {
      home.packages = launcher.packages ++ [ pkgs.omniwm ];

      # OmniWM rewrites settings.toml from its Settings window and replaces the
      # symlink with a regular file, so force re-links the tracked template on
      # the next switch instead of failing with a clobber error.
      xdg.configFile."omniwm/settings.toml" = {
        source = ./omniwm/settings.toml;
        force = true;
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
