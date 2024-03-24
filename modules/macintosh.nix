{ inputs, config, lib, pkgs, ... }:

with pkgs.stdenv;
with lib; {
  imports = [
    inputs.agenix.darwinModules.age
    inputs.home-manager.darwinModules.home-manager
  ];
  networking.hostName = config.my.hostname;
  nix = {
    daemonProcessType = "Adaptive";
    package = pkgs.nixUnstable;
    #  sysctl -n hw.ncpu
    settings.cores = 12;
    # extra-trusted-users = [ config.my.username ];
    gc = {
      user = "${config.my.username}";
      automatic = true;
      interval = { Hour = 168; };

      options = "--delete-older-than 7d";
    };
  };

  launchd.daemons.nix-daemon.serviceConfig.SoftResourceLimits.NumberOfFiles = 10240;

  system.stateVersion = 4;
  services.nix-daemon = {
    enable = true;
    logFile = "/var/log/nix-daemon.log";
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
    # remapCapsLockToControl = true;
  };

  environment.shells = [ pkgs.zsh ];
  environment.systemPackages = [ pkgs.zsh pkgs.gcc ];
  programs.bash.enable = false;
  programs.zsh = {
    enableCompletion = true;
    enable = true;
  };
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  time.timeZone = "America/Regina";

  users.users.${config.my.username} = {
    shell = pkgs.zsh;
    home = config.my.homeDirectory;
  };
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = false;
  home-manager.users.${config.my.username} =
    import ./home.nix { inherit inputs pkgs lib config; };

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
