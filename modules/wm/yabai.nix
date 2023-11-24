# modules/wm/yabai.nix
#
# this module aims to encapsulate all configurations
# to have a functional tiling window manager environment on MacOS
# yabai/skhd
{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.wm.yabai;
  emacsclient =
    if config.modules.editors.emacs.enableService then
      "${pkgs.emacs}/bin/emacsclient -c -a 'emacs'"
    else
      "doom run";
  emacsEveryWhere =
    if config.modules.editors.emacs.enableService then
      ''${pkgs.emacs}/bin/emacsclient --eval "(emacs-everywhere)"''
    else
      "doom +everywhere";

  daemonPath = "/Library/LaunchDaemons/org.nixos.yabai-sa.plist";

  # to escape $ propertly, config uses that create fsspace
  moveConfig = builtins.readFile ./skhdrc;
  # it is nice to reference pkgs full path
  laucherConfig = ''
    shift + ctrl + alt - d: ${emacsclient}
    shift + ctrl + alt - e: ${emacsEveryWhere}
    shift + ctrl + alt - x: org-capture -k n
    shift + ctrl + alt - f : open -n -a ~/.nix-profile/Applications/Firefox.app
    shift + ctrl + alt - return : open -n -a ~/.nix-profile/Applications/Alacritty.app
    shift + ctrl + alt - v: osascript -e 'tell application "Viscosity" to connect "work"'
    # reload skhd configuration
    shift + ctrl + alt - r: pkill yabai && \
                            ${pkgs.skhd}/bin/skhd -r && \
                            osascript -e 'display notification  "restart yabai and reload skhd"'
    # lock screen
    shift + ctrl + alt - l: pmset displaysleepnow
    # display current configuration
    shift + ctrl + alt - h: open /etc/skhdrc

    # take screenshot
    shift + ctrl + alt - s: screencapture -ic
    shift + ctrl + alt - i: screencapture -i /tmp/$(date +%s).png
  '';

in
{
  options.modules.wm.yabai = { enable = mkEnableOption "yabai"; };

  config = mkIf cfg.enable {
    fonts.fonts = with pkgs; [
      sketchybar-app-font
      font-hack-nerd-font
      sf-symbols
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
      # https://github.com/montchr/dotfield/blob/8bb31c05a1eb4ec76c31a0ca192368ede1ebae0a/profiles/os-specific/darwin/gui/yabai.nix
      home.packages = [
        pkgs.ical-buddy
        # pkgs.sketchybar-cpu-helper
        (

          pkgs.writeShellScriptBin "yabai-sa-kickstart" ''
            #
            # yabai-sa-kickstart
            #
            # Kickstart the scripting addition in case it fails to load.
            #
            set -x
            # See https://github.com/koekeishiya/yabai/wiki/Installing-yabai-(from-HEAD)#updating-to-latest-head
            [[ $(sudo launchctl list | grep yabai-sa) ]] && {
              sudo launchctl unload ${daemonPath}
             }
            sudo yabai --load-sa
            sudo launchctl load ${daemonPath}
            set +x
          ''
        )
        (
          pkgs.writeShellScriptBin "yabai-next-window" ''
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
          ''
        )

      ];

      xdg.configFile."sketchybar".source = ./sketchybar;
    };
    services.skhd = {
      enable = true;
      skhdConfig = ''
        # laucher configurations
        ${laucherConfig}
        # movement configurations
        ${moveConfig}
      '';
    };
    launchd.user.agents.skhd.serviceConfig = {
      StandardOutPath = "/tmp/skhd.log";
      StandardErrorPath = "/tmp/skhd.log";
    };
    launchd.user.agents.yabai.serviceConfig = {
      StandardOutPath = "/tmp/yabai.log";
      StandardErrorPath = "/tmp/yabai.log";
    };

    services.sketchybar = {
      extraPackages = [
        pkgs.jq
        pkgs.gh
        pkgs.ripgrep
        pkgs.ical-buddy
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

    services.yabai = {
      enable = true;
      package = pkgs.yabai;
      enableScriptingAddition = true;
      config = {
        window_border = "off";
        window_border_width = 2;
        active_window_border_color = "0xff81a1c1";
        normal_window_border_color = "0xff3b4252";
        focus_follows_mouse = "autoraise";
        mouse_follows_focus = "off";

        mouse_drop_action = "stack";
        window_placement = "second_child";
        window_opacity = "off";
        window_topmost = "off";
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
      extraConfig = ''
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
  };
}
