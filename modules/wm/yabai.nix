# modules/wm/yabai.nix
#
# this module aims to encapsulate all configurations
# to have a functional tiling window manager environment on MacOS
# yabai/skhd
{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.wm.yabai;
  launcher = import ./skhd-launcher.nix { inherit config pkgs; };
  # to escape $ propertly, config uses that create fsspace
  moveConfig = builtins.readFile ./skhdrc;
  # it is nice to reference pkgs full path
  reloadConfig = ''
    # reload skhd configuration
    shift + ctrl + alt - r: pkill yabai && \
                            ${pkgs.skhd}/bin/skhd -r && \
                            ${pkgs.sketchybar}/bin/sketchybar --reload && osascript -e 'display notification  "restart yabai and reload sketchybar skhd"'
  '';
in
{
  options.modules.wm.yabai = {

    enable = mkEnableOption "yabai";
    overideAppLanucher = mkOption {
      type = types.bool;
      default = false;
    };
    enableJankyborders = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      fonts.packages = with pkgs; [
        sketchybar-app-font
        font-hack-nerd-font
        sf-symbols
        emacs-all-the-icons-fonts

      ];

      home-manager.users.${config.my.username} = {
        programs.gh = {
          enable = true;
          settings = {
            git_protocol = "https";
            prompt = "enabled";
            aliases = {
              co = "pr checkout";
              pv = "pr view";
            };
          };
        };
        home.packages = launcher.packages ++ [
          (pkgs.writeShellScriptBin "yabai-next-window" ''
            #
            # yabai-next-window
            #
            # move to next window
            #
            WINDOW=$(yabai -m query --windows --window)
            STACK_INDEX=$(echo "$WINDOW" | jq '.["stack-index"]')
            if [[ $STACK_INDEX -gt 0 ]]; then
               LAST_STACK_INDEX=$(yabai -m query --windows --window stack.last | jq '.["stack-index"]')
               if [[ "$STACK_INDEX" == "$LAST_STACK_INDEX" ]]; then
                 yabai -m window --focus stack.first
               else
                 yabai -m window --focus stack.next
               fi
            else
               yabai - window --focus west
            fi
          '')

        ];

        # CoreText on some macOS installs does not reliably pick up fonts that
        # nix-darwin links as whole packages under /Library/Fonts/Nix Fonts.
        # The sketchybar app icons are ligatures from this font, so expose it
        # directly in the user's Fonts directory as well.
        home.file."Library/Fonts/sketchybar-app-font.ttf" = {
          source = "${pkgs.sketchybar-app-font}/share/fonts/truetype/sketchybar-app-font.ttf";
          force = true;
        };

        xdg.configFile."sketchybar".source = ./sketchybar;

      };
      # Grant Accessibility via tccutil (requires Filesystem Protections disabled in SIP).
      # Runs as root during darwin-rebuild switch; tccutil --insert is idempotent.
      #
      # postActivation, not a name of our own: nix-darwin executes only the
      # hard-coded list in modules/system/activation-scripts.nix, so
      # `system.activationScripts.yabaiTCCProfile` was accepted by the module
      # system and never run.
      system.activationScripts.postActivation.text = mkAfter ''
        TCCUTIL="${pkgs.tccutil-manage}/bin/tccutil-manage"
        YABAI="${pkgs.yabai}/bin/yabai"
        SKHD="${config.services.skhd.package}/bin/skhd"
        echo "Granting Accessibility to yabai..."
        "$TCCUTIL" --insert "$YABAI" || true
        # skhd needs the same grant for its event tap, and it is worse off than
        # yabai: it never checks the result of CGEventTapCreate, so an ungranted
        # binary just runs on with a NULL tap and logs nothing. Its keybindings
        # are its children, so TCC blames skhd for screencapture too.
        echo "Granting Accessibility and Screen Recording to skhd..."
        "$TCCUTIL" --insert "$SKHD" || true
        "$TCCUTIL" --service kTCCServiceScreenCapture --insert "$SKHD" || true
        # This script runs after userLaunchd, so the agent started before its own
        # grant existed. A new store path (any nixpkgs bump) restarts it with a
        # dead tap, so restart it once the row is in.
        launchctl kickstart -k "gui/$(id -u '${config.my.username}')/org.nixos.skhd" || true
        ${lib.optionalString cfg.enableJankyborders ''
          # borders also needs Accessibility, and it is silent about it the same
          # way: ungranted it stays up, its AX queries just return nothing, so a
          # new store path needs the restart too.
          BORDERS="${config.services.jankyborders.package}/bin/borders"
          echo "Granting Accessibility to jankyborders..."
          "$TCCUTIL" --insert "$BORDERS" || true
          launchctl kickstart -k "gui/$(id -u '${config.my.username}')/org.nixos.jankyborders" || true
        ''}
        ${lib.optionalString config.modules.mouseless.enable ''
          echo "Granting Accessibility to mouseless..."
          "$TCCUTIL" --insert net.sonuscape.mouseless || true
        ''}
      '';

      services.skhd = {
        enable = true;
        skhdConfig = ''
          # launcher chords shared with modules/wm/omniwm.nix
          ${launcher.config}
          ${reloadConfig}
          # movement configurations
          ${moveConfig}
        '';
      };

      launchd.user.agents.skhd.serviceConfig = {
        StandardOutPath = "/tmp/skhd.log";
        StandardErrorPath = "/tmp/skhd.log";
      };

      # skhd never re-creates its CGEventTap after sleep/wake, and as noted
      # above it never checks the result of CGEventTapCreate, so a tap lost
      # across sleep leaves the binary up, silent and deaf to every binding.
      # Nothing observable from the outside tells a live tap from a dead one,
      # so re-exec the agent on a timer instead of waiting for a report.
      # Cost: a key pressed inside the ~50ms restart window is dropped.
      launchd.user.agents.skhd-watchdog = {
        serviceConfig = {
          ProgramArguments = [
            "/bin/sh"
            "-c"
            "/bin/launchctl kickstart -k gui/$(id -u '${config.my.username}')/org.nixos.skhd"
          ];
          RunAtLoad = true;
          # in secs
          StartInterval = 900;
        };
      };
      launchd.user.agents.yabai.serviceConfig = {
        StandardOutPath = "/tmp/yabai.log";
        StandardErrorPath = "/tmp/yabai.log";
      };

      services.sketchybar = {
        extraPackages = [
          pkgs.jq
          pkgs.python3
          pkgs.gh
          pkgs.ripgrep
          pkgs.sketchybar-cpu-helper
        ];
        enable = true;
      };
      launchd.user.agents.sketchybar = {
        serviceConfig = {
          StandardOutPath = "/tmp/sketchybar.log";
          StandardErrorPath = "/tmp/sketchybar.log";
        };
      };

      services.jankyborders = {
        enable = cfg.enableJankyborders;
        width = 6.0;
        hidpi = true;
        active_color = "0xff7793d1";
        inactive_color = "0xff5e6798";
      };

      services.yabai = {
        enable = true;
        enableScriptingAddition = true;
        config = {
          focus_follows_mouse = "autoraise";
          mouse_follows_focus = "on";
          mouse_drop_action = "stack";
          window_placement = "second_child";
          window_opacity = "off";
          split_ratio = "0.50";
          auto_balance = "on";
          mouse_modifier = "fn";
          mouse_action1 = "move";
          mouse_action2 = "resize";
          layout = "bsp";
          top_padding = 45;
          # bottom_padding = 5;
          # left_padding = 10;
          # right_padding = 10;
          window_gap = 10;
        };
        # https://github.com/koekeishiya/yabai/blob/master/doc/yabai.asciidoc#signal
        # https://felixkratz.github.io/SketchyBar/config/events#triggering-custom-events
        # https://github.com/koekeishiya/yabai/wiki/Installing-yabai-(from-HEAD)
        extraConfig = ''
          yabai -m signal --add event=dock_did_restart action="sudo ${pkgs.yabai}/bin/yabai --load-sa"
          # rules
          yabai -m rule --add label="About This Mac" app="System Information" title="About This Mac" manage=off
          yabai -m rule --add app='System Preferences' manage=off
          yabai -m rule --add app='mono-stretchly' manage=off
          # Any other arbitrary config here
          yabai -m signal --add event=window_focused action="sketchybar --trigger window_focus"
          yabai -m signal --add event=window_resized action="sketchybar --trigger window_focus"
          yabai -m signal --add event=window_moved action="sketchybar --trigger window_on_spaces"
          yabai -m signal --add event=window_created action="sketchybar --trigger windows_on_spaces"
          yabai -m signal --add event=window_destroyed action="sketchybar --trigger windows_on_spaces"
        '';
      };
    })

    # The agents this module owns when yabai is off. nix-darwin leaves an agent
    # it no longer declares running, and its plist is load -w, so a leftover
    # returns at the next login: yabai tiling the same windows as
    # modules/wm/omniwm.nix, a second set of borders, sketchybar querying a
    # yabai that is gone. See modules/launchd.nix.
    (mkIf (!cfg.enable) {
      modules.launchd.userAgentsToRemove = [
        "org.nixos.yabai"
        "org.nixos.jankyborders"
        "org.nixos.sketchybar"
      ];
    })

  ];
}
