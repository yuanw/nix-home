{ inputs, config, lib, pkgs, ... }:

with pkgs.stdenv;
with lib; {

  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  system.stateVersion = 4;
  nix.maxJobs = 8;
  services.nix-daemon.enable = false;
  nixpkgs = {
    overlays = [ inputs.nur.overlay inputs.emacs.overlay (import ./overlays) ];

    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = false;
    };

  };

  system.defaults = {
    dock = {
      autohide = true;
      mru-spaces = false;
      orientation = "left";
      mineffect = "scale";
      showhidden = true;
      launchanim = false;
      show-recents = false;
      minimize-to-application = true;
      show-process-indicators = true;
      #mouse-over-hilite-stack = false;
    };

    screencapture.location = "/tmp";

    finder = {
      AppleShowAllExtensions = true;
      _FXShowPosixPathInTitle = true;
      FXEnableExtensionChangeWarning = false;
    };

    #trackpad = {
    #  Clicking = true;
    #  TrackpadThreeFingerDrag = true;
    #};

    NSGlobalDomain._HIHideMenuBar = true;
    #NSGlobalDomain."com.apple.mouse.tapBehavior" = null;
  };
  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = true;
  };

  environment.shells = [ pkgs.zsh ];
  environment.systemPackages = [ pkgs.zsh pkgs.gcc ];
  programs.bash.enable = false;
  programs.zsh.enable = true;
  time.timeZone = "America/Regina";

  users.users.yuanwang.shell = pkgs.zsh;
  users.users.yuanwang.home = "/Users/yuanwang";

  fonts.enableFontDir = true;
  fonts.fonts = with pkgs; [
    emacs-all-the-icons-fonts
    fira-code
    font-awesome
    roboto
    roboto-mono
  ];
  services.skhd = {
    enable = true;
    skhdConfig = ''
      # launchers
      shift + ctrl + alt - e: open ~/.nix-profile/Applications/Emacs.app
      shift + ctrl + alt - return : open ~/.nix-profile/Applications/Alacritty.app
      shift + ctrl + alt - v: osascript -e 'tell application "Viscosity" to connect "work"'
      # focus window
      alt - left: yabai -m window --focus west
      alt - down : yabai -m window --focus south || yabai -m display --focus prev
      alt - up : yabai -m window --focus north || yabai -m display --focus next
                   alt - right : yabai -m window --focus east
                   # shift window in current workspace, use the arrow keys
                   alt + shift - left  : yabai -m window --warp west
                   alt + shift - down  : yabai -m window --warp south
                   alt + shift - up    : yabai -m window --warp north
                   alt + shift - right : yabai -m window --warp east
       # fast focus desktop
       cmd + ctrl - tab : yabai -m space --focus recent
       cmd + ctrl - p : yabai -m space --focus prev
       cmd + ctrl - n : yabai -m space --focus next
       cmd + ctrl - 1 : yabai -m space --focus 1
       cmd + ctrl - 2 : yabai -m space --focus 2
       cmd + ctrl - 0x21 : yabai -m window --focus stack.prev # this is [
                   cmd + ctrl - 0x1E : yabai -m window --focus stack.next # this is ]
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

  # Recreate /run/current-system symlink after boot
  services.activate-system.enable = true;
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
}
