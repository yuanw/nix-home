# modules/wm/yabai.nix
#
# this module aims to encapsulate all configurations
# to have a functional tiling window manager environment on MacOS
# yabai/skhd/spacebar
{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.wm.yabai;
  # to escape $ propertly, config uses that create fsspace
  moveConfig = builtins.readFile ./skhdrc;
  # it is nice to reference pkgs full path
  laucherConfig = ''
    shift + ctrl + alt - e: open ~/.nix-profile/Applications/Emacs.app
    shift + ctrl + alt - return : open ~/.nix-profile/Applications/Alacritty.app
    shift + ctrl + alt - v: osascript -e 'tell application "Viscosity" to connect "work"'
    # reload skhd configuration
    shift + ctrl + alt - r: ${pkgs.skhd}/bin/skhd -r && \
                            ${pkgs.alerter}/alerter -message "skhd config loaded"
    # lock screen
    shift + ctrl + alt - l: pmset displaysleepnow
    # display current configuration
    shift + ctrl + alt - h: open /etc/skhdrc

  '';
in {
  options.modules.wm.yabai = { enable = mkEnableOption "yabai"; };

  config = mkIf cfg.enable {
    services.skhd = {
      enable = true;
      skhdConfig = ''
        # laucher configurations
        ${laucherConfig}
        # movement configurations
        ${moveConfig}
      '';
      #skhdConfig = builtins.readFile ./skhdrc;
    };
    services.spacebar.enable = true;
    services.spacebar.package = pkgs.spacebar;
    services.spacebar.config = {
      debug_output = "on";
      clock_format = "%R";
      # bagua eight trigrams - top bar is least significant binary digit
      space_icon_strip = "☷ ☶ ☵ ☴ ☳ ☲ ☱ ☰ ";
      text_font = "Roboto Mono:Regular:12.0";
      icon_font = "FontAwesome:Regular:12.0";
      background_color = "0xff202020";
      foreground_color = "0xffa8a8a8";
      space_icon_color = "0xff14b1ab";
      dnd_icon_color = "0xfffcf7bb";
      clock_icon_color = "0xff99d8d0";
      power_icon_color = "0xfff69e7b";
      battery_icon_color = "0xffffbcbc";
      power_icon_strip = " ";
      space_icon = "";
      clock_icon = "";
      dnd_icon = "";
    };
    services.yabai = {
      enable = true;
      package = pkgs.yabai;
      enableScriptingAddition = false;
      config = {
        focus_follows_mouse = "autoraise";
        mouse_follows_focus = "off";
        window_placement = "second_child";
        window_opacity = "off";
        window_opacity_duration = "0.0";
        window_border = "on";
        window_border_placement = "inset";
        window_border_width = 2;
        window_border_radius = 3;
        active_window_border_topmost = "off";
        window_topmost = "on";
        window_shadow = "float";
        active_window_border_color = "0xff5c7e81";
        normal_window_border_color = "0xff505050";
        insert_window_border_color = "0xffd75f5f";
        active_window_opacity = "1.0";
        normal_window_opacity = "1.0";
        split_ratio = "0.50";
        auto_balance = "on";
        mouse_modifier = "fn";
        mouse_action1 = "move";
        mouse_action2 = "resize";
        layout = "bsp";
        top_padding = 36;
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
