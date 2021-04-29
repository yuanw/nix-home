{ inputs, config, lib, pkgs, localConfig, ... }:

with pkgs.stdenv;
with lib; {
  networking.hostName = localConfig.hostname;
  nix = {
    package = pkgs.nixFlakes;
    binaryCaches = [
      "https://utdemir.cachix.org"
      "https://hs-nix-template.cachix.org"
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://emacs-osx.cachix.org"
    ];
    binaryCachePublicKeys = [
      "utdemir.cachix.org-1:mDgucWXufo3UuSymLuQumqOq1bNeclnnIEkD4fFMhsw="
      "hs-nix-template.cachix.org-1:/YbjZCrYAw7d9ayLayk7ZhBdTEkR10ZFmFuOq6ZJo4c="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "emacs-osx.cachix.org-1:Q2++pOcNsiEjmDLufCzzdquwktG3fFDYzZrd8cEj5Aw="
    ];
    trustedUsers = [ "root" localConfig.username ];
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc = {
      automatic = true;
      user = "${localConfig.username}";
    };
  };

  system.stateVersion = 4;
  nix.maxJobs = 8;
  services.nix-daemon.enable = false;
  nixpkgs = {
    overlays = [
      inputs.nur.overlay
      ## inputs.emacs.overlay
      inputs.spacebar.overlay
      (import ./overlays)
      (final: prev: {
        emacsOsxNativeTile = (inputs.emacs-osx).emacsOxsNativeTile;
      })
    ];

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

  users.nix.configureBuildUsers = true;
  users.users.${localConfig.username} = {
    shell = pkgs.zsh;
    home = localConfig.homeDirectory;
  };
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = false;
  home-manager.users.${localConfig.username} =
    import ./home.nix { inherit pkgs lib config localConfig; };

  fonts.enableFontDir = true;
  fonts.fonts = with pkgs; [
    fira-code
    font-awesome
    #iosevka
    roboto
    roboto-mono
    pragmata-pro
  ];
  # Recreate /run/current-system symlink after boot
  services.activate-system.enable = true;
}
