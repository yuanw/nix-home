# modules/wm/yabai.nix
#
# this module aims to encapsulate all configurations
# to have a functional tiling window manager environment on MacOS
# yabai/skhd/spacebar
{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.wm.yabai;
  emacsclient = "${pkgs.emacs}/bin/emacsclient -c";

  # to escape $ propertly, config uses that create fsspace
  moveConfig = builtins.readFile ./skhdrc;
  # it is nice to reference pkgs full path
  laucherConfig = ''
    shift + ctrl + alt - d: ${pkgs.emacs}/bin/emacsclient -c
    shift + ctrl + alt - e: emacsclient --eval "(emacs-everywhere)"
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
    shift + ctrl + alt - m: open ~/$HOME/moonlander.pdf

    # take screenshot
    shift + ctrl + alt - s: screencapture -ic
    shift + ctrl + alt - i: screencapture -i /tmp/$(date +%s).png
  '';

  #https://stackoverflow.com/a/64930847
  cpuStat = pkgs.writeShellScriptBin "cpuStat"
    "top -l  2 | grep -E \"^CPU\" | tail -1 | awk '{ print $3 + $5\"%\" }'";
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
    };
    services.spacebar.enable = false;
    services.spacebar.package = pkgs.spacebar;
    # https://github.com/cmacrae/spacebar/blob/master/doc/spacebar.asciidoc
    services.spacebar.config = {
      debug_output = "on";
      clock_format = "%R";
      display = "all";
      position = "top";
      title = "on";
      # bagua eight trigrams - top bar is least significant binary digit
      # space_icon_strip = "☷ ☶ ☵ ☴ ☳ ☲ ☱ ☰";
      space_icon_strip = "向 死 而 生 逆 水 行 舟";
      #text_font = ''"Roboto Mono:Regular:12.0"'';
      #icon_font = ''"Font Awesome 5 Free:Solid:12.0"'';
      background_color = "0xff222222";
      foreground_color = "0xffd8dee9";
      space_icon_color = "0xffffab91";
      dnd_icon_color = "0xffd8dee9";
      clock_icon_color = "0xffd8dee9";
      power_icon_color = "0xffd8dee9";
      battery_icon_color = "0xffd8dee9";
      power_icon_strip = " ";
      space_icon = "•";
      spaces_for_all_displays = "on";
      display_separator = "on";
      display_separator_icon = "";
      space_icon_color_secondary = "0xff78c4d4";
      space_icon_color_tertiary = "0xfffff9b0";
      clock_icon = "";
      dnd_icon = "";
      # f2db chip
      right_shell_icon = "";
      right_shell = "on";
      right_shell_icon_color = "0xffd8dee9";
      right_shell_command = "${cpuStat}/bin/cpuStat";
    };
    launchd.user.agents.spacebar.serviceConfig = {
      StandardErrorPath = "/tmp/spacebar.err.log";
      StandardOutPath = "/tmp/spacebar.out.log";
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
        window_border = "on";
        window_border_width = 2;
        active_window_border_color = "0xff81a1c1";
        normal_window_border_color = "0xff3b4252";
        focus_follows_mouse = "autoraise";
        mouse_follows_focus = "on";
        mouse_drop_action = "stack";
        window_placement = "second_child";
        window_opacity = "off";
        window_topmost = "on";
        window_shadow = "float";
        active_window_opacity = "1.0";
        normal_window_opacity = "1.0";
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
