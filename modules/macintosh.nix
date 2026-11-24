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
{
  imports = [
    inputs.agenix.darwinModules.age
    inputs.fast-nix-gc.darwinModules.default
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

  # GC and store dedup are the fast-nix-gc daemons (github:Mic92/fast-nix-gc):
  # one pass over ValidPaths/Refs into an adjacency list instead of a SQLite
  # query per store path, parallel deletion and hardlinking.  They replace the
  # hand-rolled `nix-collect-garbage'/`nix-store --optimise' daemons, and the
  # user agent that used to delete the user's own generations: the binary reads
  # the SQLite DB and unlinks /nix/store paths directly rather than asking the
  # daemon, so it only works as root -- both jobs now run in the one root
  # daemon, which is also why gcRootsDirs and XDG_STATE_HOME below are needed.
  services.fast-nix-gc = {
    enable = true;
    # `automatic' is left off repo-wide and the schedule is set on the plist
    # below instead: the module asserts nix.enable whenever automatic is on,
    # and mist/ci install nix themselves (hosts/mist.nix: nix.enable = false),
    # so one `automatic = true' here would fail to build those two hosts.
    # StartCalendarInterval is all `automatic' sets, so the plist is identical
    # either way -- and the binary needs nix on PATH only to read
    # keep-outputs/keep-derivations, which the module's PATH fallback
    # (/nix/var/nix/profiles/default/bin) covers for an installer-managed nix.
    deleteOlderThan = "3d";
    # The binary scans the state-dir gcroots and profiles only -- what lives
    # under /nix/var/nix -- while a `nix-store --gc' run by the user also
    # treats $XDG_STATE_HOME/nix/{gcroots,profiles} as roots.  That is where
    # ~/.nix-profile and the prefetch-work-sources pins live, and
    # scripts/prefetch-work-sources.sh relies on those pins surviving a GC, so
    # pass them: root's root set is then a superset of the user's.
    # shortcut: a dry run here found the same 23326 dead paths with and
    # without these two dirs (everything in them is also reachable through
    # gcroots/auto links and open files), so this is insurance, not a fix for
    # something seen broken.  Re-run `fast-nix-gc --dry-run' as root after
    # changing where this user's roots live.
    gcRootsDirs = [
      "${config.my.homeDirectory}/.local/state/nix/profiles"
      "${config.my.homeDirectory}/.local/state/nix/gcroots"
    ];
  };

  # Which generations to delete comes from state-dir/profiles plus
  # $XDG_STATE_HOME/nix/profiles.  Under launchd HOME is /var/root and
  # XDG_STATE_HOME is unset, so without this only root's profiles age out and
  # every user generation -- kept as a root by gcRootsDirs above -- would pin
  # its closure in the store forever.
  #
  # The module writes no logs, and launchd's default (os_log) is not where the
  # previous GC daemons were read, so keep the same /tmp files.
  launchd.daemons.fast-nix-gc.serviceConfig = {
    StartCalendarInterval = [
      {
        Weekday = 0;
        Hour = 14;
        Minute = 0;
      }
    ];
    EnvironmentVariables.XDG_STATE_HOME = "${config.my.homeDirectory}/.local/state";
    StandardErrorPath = "/tmp/daemons-nix-gc.log";
    StandardOutPath = "/tmp/daemons-nix-gc.log";
  };

  services.fast-nix-optimise = {
    enable = true;
  };

  launchd.daemons.fast-nix-optimise.serviceConfig = {
    StartCalendarInterval = [
      {
        Weekday = 0;
        Hour = 15;
        Minute = 0;
      }
    ];
    StandardErrorPath = "/tmp/daemons-nix-store-optimise.log";
    StandardOutPath = "/tmp/daemons-nix-store-optimise.log";
  };

  # `just switch` needs root (darwin-rebuild refuses to activate as a normal
  # user), so this one command skips the password prompt.  Everything else still
  # authenticates.
  #
  # The flags are part of the rule rather than a `--flake *` wildcard: darwin-rebuild
  # pointed at any other flake activates it as root, so a wildcard would be a general
  # root grant for anything running as this user.  No NIX_CONFIG has to cross the
  # sudo because root reads the experimental features from /etc/nix/nix.custom.conf
  # (modules/nix-custom-conf.nix).
  #
  # The first switch after adding this still asks for the password: the rule itself
  # lands in /etc/sudoers.d as part of that activation.
  security.sudo.extraConfig = lib.mkAfter ''
    ${config.system.primaryUser} ALL=(root) NOPASSWD: /run/current-system/sw/bin/darwin-rebuild switch --flake . --fallback
  '';

  fonts.packages = with pkgs; [
    fira-code
    font-awesome
    stable.aporetic
    roboto
    roboto-mono
  ];
}
