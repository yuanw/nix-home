{ inputs, config, lib, pkgs, ... }:

with pkgs.stdenv;
with lib; {
  networking.hostName = config.my.hostname;
  nix = {
    package = pkgs.nixUnstable;

    configureBuildUsers = true;
    settings = {
      substituters = [
        "https://utdemir.cachix.org"
        "https://hs-nix-template.cachix.org"
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://cachix.org/api/v1/cache/yuanwang-wf"
        "https://cachix.org/api/v1/cache/emacs"
      ];
      trusted-public-keys = [
        "utdemir.cachix.org-1:mDgucWXufo3UuSymLuQumqOq1bNeclnnIEkD4fFMhsw="
        "hs-nix-template.cachix.org-1:/YbjZCrYAw7d9ayLayk7ZhBdTEkR10ZFmFuOq6ZJo4c="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "yuanwang-wf.cachix.org-1:P/RZ5Iuuuv2MYCNCnAsLfPGmgKMKeTwPaJclkrcwx80="
        "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
      ];

      max-jobs = 8;
      trusted-users = [ "root" config.my.username ];
    };
    # Avoid unwanted garbage collection when using nix-direnv
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs          = true
      keep-derivations      = true
    '';
    # trustedBinaryCaches = config.nix.binaryCaches;
    gc = {
      automatic = true;
      user = "${config.my.username}";
      interval = { Hour = 24 * 7; };
    };
  };

  system.stateVersion = 4;
  services.nix-daemon.enable = true;
  nixpkgs = {
    overlays = [
      inputs.nur.overlay
      # inputs.mac-emacs.overlay
      #       (final: prev:
      #         let inherit (prev) lib;
      #             overlays = [
      #               (self: super: {
      #                 haskellPackages = super.haskellPackages.override {
      #                   overrides = hself: hsuper: {
      #                     Agda = hsuper.Agda.overrideAttrs (old: {
      #                       postInstall = "";
      #                     });
      #                   };
      #                 };
      #               })
      #               inputs.agda.overlay
      #             ];
      #               composed = lib.composeManyExtensions overlays;

      # in composed final prev
      #       )
      (import ./overlays)
      (final: prev: {
        resource-id = inputs.resource-id.defaultPackage.x86_64-darwin;
        ws-access-token = inputs.ws-access-token.defaultPackage.x86_64-darwin;
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

  users.users.${config.my.username} = {
    shell = pkgs.zsh;
    home = config.my.homeDirectory;
  };
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = false;
  home-manager.users.${config.my.username} =
    import ./home.nix { inherit pkgs lib config; };

  fonts.fontDir.enable = true;
  fonts.fonts = with pkgs; [
    fira-code
    font-awesome
    #iosevka
    roboto
    roboto-mono
    # pragmata-pro
  ];
  # Recreate /run/current-system symlink after boot
  services.activate-system.enable = true;
}
