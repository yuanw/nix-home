{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.stdenv;
with lib;
{
  imports = [
    inputs.agenix.darwinModules.age
    inputs.home-manager.darwinModules.home-manager
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
      user = "${config.my.username}";
      automatic = true;
      interval = {
        Hour = 22;
      };

      options = "--delete-older-than 7d";
    };
  };

  launchd.daemons.nix-daemon.serviceConfig.SoftResourceLimits.NumberOfFiles = 10240;
  #    curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- repair sequoia --move-existing-users
  #ids.uids.nixbld = 300;
  system.stateVersion = 5;
  system.activationScripts.postActivation.text = ''
    printf "disabling spotlight indexing... "
    mdutil -i off -d / &> /dev/null
    mdutil -E / &> /dev/null
    echo "ok"
  '';
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

  users.users.${config.my.username} = {
    shell = pkgs.zsh;
    home = config.my.homeDirectory;
  };
  home-manager.backupFileExtension = "backup";
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = false;
  home-manager.users.${config.my.username} = import ./home.nix {
    inherit
      inputs
      pkgs
      lib
      config
      ;
  };

  fonts.packages = with pkgs; [
    fira-code
    font-awesome
    #iosevka
    roboto
    roboto-mono
  ];
}
