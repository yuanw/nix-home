{
  inputs,
  config,
  lib,
  pkgs,
  inputs',
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
      FXDefaultSearchScope = "SCcf"; # search current folder, not iCloud
      FXPreferredViewStyle = "Nlsv";
      ShowPathbar = true;
    };

    #am I sure about want to open an app downloaded from the internet
    LaunchServices.LSQuarantine = false;

    #trackpad = {
    #  Clicking = true;
    #  TrackpadThreeFingerDrag = true;
    #};

    NSGlobalDomain = {
      _HIHideMenuBar = true;
      # Don't save to iCloud by default
      NSDocumentSaveNewDocumentsToCloud = false;
      # Disable auto-correct / type-to-search phoning home
      NSAutomaticSpellingCorrectionEnabled = false;
    };
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
  # GPG agent is managed by home-manager (modules/home/gpg.nix).
  programs.gnupg.agent.enable = false;
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
      inputs'
      ;
  };

  launchd.daemons.nix-gc = {
    command = "${nixPackage}/bin/nix-collect-garbage --delete-older-than 3d";
    serviceConfig = {
      RunAtLoad = false;
      KeepAlive = false;
      StartCalendarInterval = [
        {
          Weekday = 0;
          Hour = 3;
          Minute = 15;
        }
      ];
      StandardErrorPath = "/tmp/daemons-nix-gc.log";
      StandardOutPath = "/tmp/daemons-nix-gc.log";
    };
  };

  launchd.daemons.nix-store-optimise = {
    command = "${nixPackage}/bin/nix-store --optimise";
    serviceConfig = {
      RunAtLoad = false;
      KeepAlive = false;
      StartCalendarInterval = [
        {
          Weekday = 0;
          Hour = 3;
          Minute = 45;
        }
      ];
      StandardErrorPath = "/tmp/daemons-nix-store-optimise.log";
      StandardOutPath = "/tmp/daemons-nix-store-optimise.log";
    };
  };

  #   environment.etc."sudoers.d/nix-collect-garbage".source = pkgs.runCommand "sudoers-nix-collect-garbage" {} ''
  #   YABAI_BIN="${nixPackage}/bin/nix-collect-garbage"
  #   SHASUM=$(sha256sum "$YABAI_BIN" | cut -d' ' -f1)
  #   cat <<EOF >"$out"
  #   %admin ALL=(root) NOPASSWD: sha256:$SHASUM $YABAI_BIN --delete-older-than 3d
  #   EOF
  # '';

  launchd.user.agents.user-nix-gc = {
    command = "${nixPackage}/bin/nix-collect-garbage --delete-older-than 3d";
    environment.NIX_REMOTE = "daemon";
    serviceConfig = {
      RunAtLoad = false;
      KeepAlive = false;
      ProcessType = "Background";
      StartCalendarInterval = [
        {
          Weekday = 0;
          Hour = 4;
          Minute = 0;
        }
      ];
      StandardErrorPath = "/tmp/user-nix-gc.log";
      StandardOutPath = "/tmp/user-nix-gc.log";
    };
  };

  fonts.packages = with pkgs; [
    fira-code
    font-awesome
    aporetic
    roboto
    roboto-mono
  ];
}
