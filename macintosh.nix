{ flake, config, lib, pkgs, ... }:

with pkgs.stdenv;
with lib; {
  networking.hostName = flake.config.my.hostname;
  nix = {
    package = pkgs.nixUnstable;
    # extra-trusted-users = [ config.my.username ];
    gc = {
      user = "${flake.config.my.username}";
      automatic = true;
      interval = { Hour = 168; };

      options = "--delete-older-than 7d";
    };
  };

  system.stateVersion = 4;
  services.nix-daemon.enable = true;
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
    # remapCapsLockToControl = true;
  };

  environment.shells = [ pkgs.zsh ];
  environment.systemPackages = [ pkgs.zsh pkgs.gcc ];
  programs.bash.enable = false;
  programs.zsh = {
    enableCompletion = false;
    enable = true;
  };
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  time.timeZone = "America/Regina";

  users.users.${flake.config.my.username} = {
    shell = pkgs.zsh;
    home = flake.config.my.homeDirectory;
  };
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = false;
  home-manager.users.${flake.config.my.username} =
    import ./home.nix { inherit flake pkgs lib config; };

  fonts.fontDir.enable = true;
  fonts.fonts = with pkgs; [
    fira-code
    font-awesome
    #iosevka
    roboto
    roboto-mono
  ];
  # Recreate /run/current-system symlink after boot
  services.activate-system.enable = true;
}
