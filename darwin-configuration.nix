{ config, lib, pkgs, ... }:
let

in with pkgs.stdenv;
with lib; {
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  nixpkgs.overlays = let path = ./overlays;
  in with builtins;
  map (n: import (path + ("/" + n))) (filter (n:
    match ".*\\.nix" n != null
    || pathExists (path + ("/" + n + "/default.nix")))
    (attrNames (readDir path)));
  environment.systemPackages = [ ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin-configuration.nix
  environment.darwinConfig = "$HOME/.config/nixpkgs/darwin-configuration.nix";

  # this is computer level Font, there is also user level font. Nix-darwin cannot manage them yet
  fonts = {
    enableFontDir = true;
    fonts = [
      pkgs.material-design-icons
      pkgs.weather-icons
      pkgs.font-awesome
      pkgs.emacs-all-the-icons-fonts
      pkgs.pragmata-pro-font
    ];
  };

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;
  services.lorri = { enable = true; };
  services.skhd = {
    enable = true;
    skhdConfig = ''
      cmd - e : open ~/.nix-profile/Applications/Emacs.app

      # focus window
      cmd + ctrl - h : yabai -m window --focus west
      cmd + ctrl - j : yabai -m window --focus south || yabai -m display --focus prev
      cmd + ctrl - k : yabai -m window --focus north || yabai -m display --focus next
      cmd + ctrl - l : yabai -m window --focus east
      cmd + ctrl - 0x21 : yabai -m window --focus stack.prev # this is [
      cmd + ctrl - 0x1E : yabai -m window --focus stack.next # this is ]
          '';
  };

  services.spacebar.enable = true;
  services.spacebar.package = pkgs.spacebar;
  services.spacebar.config = {
    debug_output = "on";
    clock_format = "%R";
    space_icon_strip = "    ";
    text_font = ''"Essential PragmataPro:Regular:12.0"'';
    icon_font = ''"FontAwesome:Regular:12.0"'';
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
    enableScriptingAddition = true;
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

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true; # default shell on catalina
  programs.bash.enable = false;
  time.timeZone = "America/Regina";

  system.defaults = {
    dock = {
      autohide = true;
      mru-spaces = false;
      minimize-to-application = true;
    };

    screencapture.location = "/tmp";

    finder = {
      AppleShowAllExtensions = true;
      _FXShowPosixPathInTitle = true;
      FXEnableExtensionChangeWarning = false;
    };

    trackpad = {
      Clicking = true;
      TrackpadThreeFingerDrag = true;
    };

    NSGlobalDomain._HIHideMenuBar = true;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
