{ config, lib, pkgs, ... }:

with lib;
let cfg = config.programs.wm;
in {
  options.programs.wm = { enable = mkEnableOption "wm"; };

  config = mkIf cfg.enable {
    services.skhd = {
      enable = true;
      skhdConfig = ''
        # launchers
        shift + ctrl + alt - e: open ~/.nix-profile/Applications/Emacs.app
        shift + ctrl + alt - return : open ~/.nix-profile/Applications/Alacritty.app
        shift + ctrl + alt - v: osascript -e 'tell application "Viscosity" to connect "work"'
        # lock screen
        shift + ctrl + alt - l: pmset displaysleepnow


        # focus window
        alt - h : yabai -m window --focus west
        alt - j : yabai -m window --focus south
        alt - k : yabai -m window --focus north
        alt - l : yabai -m window --focus east

        # swap window
        shift + alt - h : yabai -m window --swap west
        shift + alt - j : yabai -m window --swap south
        shift + alt - k : yabai -m window --swap north
        shift + alt - l : yabai -m window --swap east

        # move window
        shift + cmd - h : yabai -m window --warp west
        shift + cmd - j : yabai -m window --warp south
        shift + cmd - k : yabai -m window --warp north
        shift + cmd - l : yabai -m window --warp east

        # fast focus desktop
        cmd + alt - x : yabai -m space --focus recent
        cmd + alt - z : yabai -m space --focus prev
        cmd + alt - c : yabai -m space --focus next
        cmd + alt - 1 : yabai -m space --focus 1
        cmd + alt - 2 : yabai -m space --focus 2
        cmd + alt - 3 : yabai -m space --focus 3
        cmd + alt - 4 : yabai -m space --focus 4
        cmd + alt - 5 : yabai -m space --focus 5
        cmd + alt - 6 : yabai -m space --focus 6
        cmd + alt - 7 : yabai -m space --focus 7
        cmd + alt - 8 : yabai -m space --focus 8
        cmd + alt - 9 : yabai -m space --focus 9
        cmd + alt - 0 : yabai -m space --focus 10
                      '';
    };
    services.spacebar.enable = true;
    services.spacebar.package = pkgs.spacebar;
    services.spacebar.config = {
      debug_output = "on";
      clock_format = "%R";
      space_icon_strip = "I II III IV V";
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
