{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.stdenv;
with lib;
let
  nixPackage =
    if config.nix.enable && config.nix.package != null then config.nix.package else pkgs.nix;
in
{
  imports = [
    inputs.agenix.darwinModules.age
    # inputs.home-manager.darwinModules.home-manager
  ];
  networking.hostName = config.my.hostname;
  # https://nixos.wiki/wiki/Enterprise
  nix = {
    extraOptions = ''
      netrc-file = /etc/nix/netrc
    '';
    daemonProcessType = "Adaptive";
    # package = pkgs.nixVersions.git;
    #  sysctl -n hw.ncpu
    settings.cores = 12;
    # extra-trusted-users = [ config.my.username ];
    gc = {
      #automatic = true;
      options = "--delete-older-than 3d";
    };
  };

  system.stateVersion = 5;
  system.activationScripts.postActivation.text = ''
    printf "disabling spotlight indexing... "
    mdutil -i off -d / &> /dev/null
    mdutil -E / &> /dev/null
    echo "ok"
  '';
  services.nix-daemon = {
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
  environment.systemPackages = [
    pkgs.zsh
    pkgs.gcc
  ];
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
  system.primaryUser = config.my.username;
  users.users.${config.my.username} = {
    shell = pkgs.zsh;
    home = config.my.homeDirectory;
  };
  #home-manager.backupFileExtension = "backup";
  # home-manager.useGlobalPkgs = true;
  # home-manager.useUserPackages = false;
  home-manager.users.${config.my.username} = import ./home.nix {
    inherit
      inputs
      pkgs
      lib
      config
      ;
  };

  launchd.daemons.nix-gc = {
    command = "${nixPackage}/bin/nix-collect-garbage  --delete-older-than 3d";
    serviceConfig.RunAtLoad = true;
    serviceConfig.StartCalendarInterval = [
      {
        Weekday = 7;
        Hour = 3;
        Minute = 15;
      }
    ];
    serviceConfig.StandardErrorPath = "/tmp/daemons-nix-gc.log";
    serviceConfig.StandardOutPath = "/tmp/daemons-nix-gc.log";
  };

  launchd.agents.nix-gc = {
    command = "${nixPackage}/bin/nix-collect-garbage  --delete-older-than 3d";
    serviceConfig.RunAtLoad = false;
    serviceConfig.StartCalendarInterval = [
      {
        Weekday = 7;
        Hour = 4;
        Minute = 15;
      }
    ];
    serviceConfig.StandardErrorPath = "/tmp/user-nix-gc.log";
    serviceConfig.StandardOutPath = "/tmp/user-nix-gc.log";
  };

  fonts.packages = with pkgs; [
    fira-code
    font-awesome
    #iosevka
    roboto
    roboto-mono
  ];
}
