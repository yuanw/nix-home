# modules/wm/yabai.nix
#
# this module aims to encapsulate all configurations
# to have a functional tiling window manager environment on MacOS
# yabai/skhd
{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.wm.yabai;
  emacsclient = if config.modules.editors.emacs.enableService then
    "${pkgs.emacs}/bin/emacsclient -c"
  else
    "~/.emacs.d/bin/doom run";
  emacsEveryWhere = if config.modules.editors.emacs.enableService then
    ''${pkgs.emacs}/bin/emacsclient --eval "(emacs-everywhere)"''
  else
    "~/.emacs.d/bin/doom +everywhere";

  daemonPath = "/Library/LaunchDaemons/org.nixos.yabai-sa.plist";

  # to escape $ propertly, config uses that create fsspace
  moveConfig = builtins.readFile ./skhdrc;
  # it is nice to reference pkgs full path
  laucherConfig = ''
    shift + ctrl + alt - d: ${emacsclient}
    shift + ctrl + alt - e: ${emacsEveryWhere}https://meet.google.com/jec-riyc-ptu
    shift + ctrl + alt - return : open -n -a ~/.nix-profile/Applications/Alacritty.app
    shift + ctrl + alt - v: osascript -e 'tell application "Viscosity" to connect "work"'
    # reload skhd configuration
    shift + ctrl + alt - r: pkill yabai && \
                            ${pkgs.skhd}/bin/skhd -r && \
                            ${pkgs.alerter}/alerter -message "restart yabai and reload skhd"
    # lock screen
    shift + ctrl + alt - l: pmset displaysleepnow
    # display current configuration
    shift + ctrl + alt - h: open /etc/skhdrc

    # display moonlander configuration
    shift + ctrl + alt - m: open ~/moonlander.pdf

    # take screenshot
    shift + ctrl + alt - s: screencapture -ic
    shift + ctrl + alt - i: screencapture -i /tmp/$(date +%s).png
  '';

in {
  options.modules.wm.yabai = { enable = mkEnableOption "yabai"; };

  config = mkIf cfg.enable {

    home-manager.users.${config.my.username} = {
      # https://github.com/montchr/dotfield/blob/8bb31c05a1eb4ec76c31a0ca192368ede1ebae0a/profiles/os-specific/darwin/gui/yabai.nix
      home.packages = [
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
        #     }
            sudo yabai --uninstall-sa
            sudo yabai --install-sa
            sudo launchctl load ${daemonPath}
        #     set +x
          '')
      ];
      programs = {
        zsh = { sessionVariables = { ALERTER_HOME = "${pkgs.alerter}"; }; };
      };
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
      StandardOutPath = "/tmp/skhd.out.log";
      StandardErrorPath = "/tmp/skhd.err.log";
    };
    launchd.user.agents.yabai.serviceConfig = {
      StandardOutPath = "/tmp/yabai.out.log";
      StandardErrorPath = "/tmp/yabai.err.log";
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
        top_padding = 10;
        bottom_padding = 10;
        left_padding = 10;
        right_padding = 10;
        window_gap = 10;
      };

      extraConfig = ''
        # rules
        yabai -m rule --add app='System Preferences' manage=off
        # Any other arbitrary config here
      '';
    };
  };
}
