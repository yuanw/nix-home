{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.wm.yabai;
  moveConfig = builtins.readFile ./skhdrc;
  laucherConfig = ''
    # launchers
    shift + ctrl + alt - e: open ~/.nix-profile/Applications/Emacs.app
    shift + ctrl + alt - return : open ~/.nix-profile/Applications/Alacritty.app
    shift + ctrl + alt - v: osascript -e 'tell application "Viscosity" to connect "work"'
    # reload skhd configuration
    shift + ctrl + alt - r: "${pkgs.skhd}/bin/skhd -r"
    # lock screen
    shift + ctrl + alt - l: pmset displaysleepnow
  '';
in {
  options.programs.wm.yabai = { enable = mkEnableOption "wm"; };

  config = mkIf cfg.enable {
    services.skhd = {
      enable = true;
      skhdConfig = ''
        ${launcherConfig}
        ${moveConfig}
      '';
      #skhdConfig = builtins.readFile ./skhdrc;
      # skhdConfig = ''
      #   # launchers
      #   shift + ctrl + alt - e: open ~/.nix-profile/Applications/Emacs.app
      #   shift + ctrl + alt - return : open ~/.nix-profile/Applications/Alacritty.app
      #   shift + ctrl + alt - v: osascript -e 'tell application "Viscosity" to connect "work"'
      #   # lock screen
      #   shift + ctrl + alt - l: pmset displaysleepnow

      #   # focus window
      #   alt - h : yabai -m window --focus west
      #   alt - j : yabai -m window --focus south
      #   alt - k : yabai -m window --focus north
      #   alt - l : yabai -m window --focus east

      #   # swap window
      #   shift + alt - h : yabai -m window --swap west
      #   shift + alt - j : yabai -m window --swap south
      #   shift + alt - k : yabai -m window --swap north
      #   shift + alt - l : yabai -m window --swap east

      #   # move window
      #   shift + cmd - h : yabai -m window --warp west
      #   shift + cmd - j : yabai -m window --warp south
      #   shift + cmd - k : yabai -m window --warp north
      #   shift + cmd - l : yabai -m window --warp east

      #   # balance size of windows
      #   shift + alt - 0 : yabai -m space --balance

      #   # make floating window fill screen
      #   shift + alt - up     : yabai -m window --grid 1:1:0:0:1:1

      #   # make floating window fill left-half of screen
      #   shift + alt - left   : yabai -m window --grid 1:2:0:0:1:1

      #   # make floating window fill right-half of screen
      #   shift + alt - right  : yabai -m window --grid 1:2:1:0:1:1

      #   # create desktop, move window and follow focus - uses jq for parsing json
      #   shift + cmd - n : yabai -m space --create && \
      #                     index="$(yabai -m query --displays --display | jq '.spaces[-1]')" && \
      #                     yabai -m window --space ''${index} && \
      #                     yabai -m space --focus ''${index}

      #   # create desktop and follow focus - uses jq for parsing json
      #   cmd + alt - n : yabai -m space --create;\
      #                   index="$(yabai -m query --displays --display | jq '.spaces[-1]')" && \
      #                   yabai -m space --focus ''${index}

      #   # destroy desktop
      #   cmd + alt - w : yabai -m space --destroy

      #   # fast focus desktop
      #   cmd + alt - x : yabai -m space --focus recent
      #   cmd + alt - z : yabai -m space --focus prev
      #   cmd + alt - c : yabai -m space --focus next
      #   cmd + alt - 1 : yabai -m space --focus 1
      #   cmd + alt - 2 : yabai -m space --focus 2
      #   cmd + alt - 3 : yabai -m space --focus 3
      #   cmd + alt - 4 : yabai -m space --focus 4
      #   cmd + alt - 5 : yabai -m space --focus 5
      #   cmd + alt - 6 : yabai -m space --focus 6
      #   cmd + alt - 7 : yabai -m space --focus 7
      #   cmd + alt - 8 : yabai -m space --focus 8
      #   cmd + alt - 9 : yabai -m space --focus 9
      #   cmd + alt - 0 : yabai -m space --focus 10

      #   # send window to desktop and follow focus
      #   shift + cmd - x : yabai -m window --space recent; yabai -m space --focus recent
      #   shift + cmd - z : yabai -m window --space prev; yabai -m space --focus prev
      #   shift + cmd - c : yabai -m window --space next; yabai -m space --focus next
      #   shift + cmd - 1 : yabai -m window --space  1; yabai -m space --focus 1
      #   shift + cmd - 2 : yabai -m window --space  2; yabai -m space --focus 2
      #   shift + cmd - 3 : yabai -m window --space  3; yabai -m space --focus 3
      #   shift + cmd - 4 : yabai -m window --space  4; yabai -m space --focus 4
      #   shift + cmd - 5 : yabai -m window --space  5; yabai -m space --focus 5
      #   shift + cmd - 6 : yabai -m window --space  6; yabai -m space --focus 6
      #   shift + cmd - 7 : yabai -m window --space  7; yabai -m space --focus 7
      #   shift + cmd - 8 : yabai -m window --space  8; yabai -m space --focus 8
      #   shift + cmd - 9 : yabai -m window --space  9; yabai -m space --focus 9
      #   shift + cmd - 0 : yabai -m window --space 10; yabai -m space --focus 10

      #   # focus monitor
      #   ctrl + alt - x  : yabai -m display --focus recent
      #   ctrl + alt - z  : yabai -m display --focus prev
      #   ctrl + alt - c  : yabai -m display --focus next
      #   ctrl + alt - 1  : yabai -m display --focus 1
      #   ctrl + alt - 2  : yabai -m display --focus 2
      #   ctrl + alt - 3  : yabai -m display --focus 3

      #   # send window to monitor and follow focus
      #   ctrl + cmd - x  : yabai -m window --display recent; yabai -m display --focus recent
      #   ctrl + cmd - z  : yabai -m window --display prev; yabai -m display --focus prev
      #   ctrl + cmd - c  : yabai -m window --display next; yabai -m display --focus next
      #   ctrl + cmd - 1  : yabai -m window --display 1; yabai -m display --focus 1
      #   ctrl + cmd - 2  : yabai -m window --display 2; yabai -m display --focus 2
      #   ctrl + cmd - 3  : yabai -m window --display 3; yabai -m display --focus 3

      #   # move window
      #   shift + ctrl - a : yabai -m window --move rel:-20:0
      #   shift + ctrl - s : yabai -m window --move rel:0:20
      #   shift + ctrl - w : yabai -m window --move rel:0:-20
      #   shift + ctrl - d : yabai -m window --move rel:20:0

      #   # increase window size
      #   shift + alt - a : yabai -m window --resize left:-20:0
      #   shift + alt - s : yabai -m window --resize bottom:0:20
      #   shift + alt - w : yabai -m window --resize top:0:-20
      #   shift + alt - d : yabai -m window --resize right:20:0

      #   # decrease window size
      #   shift + cmd - a : yabai -m window --resize left:20:0
      #   shift + cmd - s : yabai -m window --resize bottom:0:-20
      #   shift + cmd - w : yabai -m window --resize top:0:20
      #   shift + cmd - d : yabai -m window --resize right:-20:0

      #   # set insertion point in focused container
      #   ctrl + alt - h : yabai -m window --insert west
      #   ctrl + alt - j : yabai -m window --insert south
      #   ctrl + alt - k : yabai -m window --insert north
      #   ctrl + alt - l : yabai -m window --insert east

      #   # rotate tree
      #   alt - r : yabai -m space --rotate 90

      #   # mirror tree y-axis
      #   alt - y : yabai -m space --mirror y-axis

      #   # mirror tree x-axis
      #   alt - x : yabai -m space --mirror x-axis

      #   # toggle desktop offset
      #   alt - a : yabai -m space --toggle padding; yabai -m space --toggle gap

      #   # toggle window parent zoom
      #   alt - d : yabai -m window --toggle zoom-parent

      #   # toggle window fullscreen zoom
      #   alt - f : yabai -m window --toggle zoom-fullscreen

      #   # toggle window native fullscreen
      #   shift + alt - f : yabai -m window --toggle native-fullscreen

      #   # toggle window border
      #   shift + alt - b : yabai -m window --toggle border

      #   # toggle window split type
      #   alt - e : yabai -m window --toggle split

      #   # float / unfloat window and center on screen
      #   alt - t : yabai -m window --toggle float;\
      #             yabai -m window --grid 4:4:1:1:2:2

      #   # toggle sticky
      #   alt - s : yabai -m window --toggle sticky

      #   # toggle sticky, float and resize to picture-in-picture size
      #   alt - p : yabai -m window --toggle sticky;\
      #             yabai -m window --grid 5:5:4:0:1:1

      #   # change layout of desktop
      #   ctrl + alt - a : yabai -m space --layout bsp
      #   ctrl + alt - d : yabai -m space --layout float
      #                 '';
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
