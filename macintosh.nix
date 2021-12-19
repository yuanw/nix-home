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
      "https://cachix.org/api/v1/cache/emacs"
    ];
    binaryCachePublicKeys = [
      "utdemir.cachix.org-1:mDgucWXufo3UuSymLuQumqOq1bNeclnnIEkD4fFMhsw="
      "hs-nix-template.cachix.org-1:/YbjZCrYAw7d9ayLayk7ZhBdTEkR10ZFmFuOq6ZJo4c="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
    ];
    trustedUsers = [ "root" localConfig.username ];
    extraOptions = ''
      experimental-features = nix-command flakes
      allow-import-from-derivation = true
    '';
    trustedBinaryCaches = config.nix.binaryCaches;
    gc = {
      automatic = true;
      user = "${localConfig.username}";
      interval = { Hour = 24 * 7; };
    };
  };

  system.stateVersion = 4;
  nix.maxJobs = 8;
  services.nix-daemon.enable = true;
  nixpkgs = {
    overlays = [
      inputs.nur.overlay
      inputs.mac-emacs.overlay
      inputs.spacebar.overlay
      (import ./overlays)
      (final: prev: {
        resource-id = inputs.resource-id.defaultPackage.x86_64-darwin;
        ws-access-token = inputs.ws-access-token.defaultPackage.x86_64-darwin;
        # haskell-language-server =
        #   inputs.hls.packages.haskell-language-server.x86_64-darwin;
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
  programs.zsh = {
    enableCompletion = false;
    enable = true;
  };
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  time.timeZone = "America/Regina";

  launchd.user.agents.yabai.serviceConfig.StandardErrorPath =
    "/tmp/yabai.err.log";
  launchd.user.agents.yabai.serviceConfig.StandardOutPath =
    "/tmp/yabai.out.log";
  launchd.user.agents.spacebar.serviceConfig.StandardErrorPath =
    "/tmp/spacebar.err.log";
  launchd.user.agents.spacebar.serviceConfig.StandardOutPath =
    "/tmp/spacebar.out.log";
  launchd.user.agents.skhd.serviceConfig.StandardOutPath = "/tmp/skhd.out.log";
  launchd.user.agents.skhd.serviceConfig.StandardErrorPath =
    "/tmp/skhd.err.log";

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
