{ inputs, inputs', config, ... }:
{
  nix = {
    # configureBuildUsers = true;
    settings = {
      trusted-users = [ "root" config.my.username ];
      substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://yuanw-nix-home-macos.cachix.org"
        "https://cachix.org/api/v1/cache/yuanwang-wf"
        "https://cachix.org/api/v1/cache/devenv"
      ];
      trusted-substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://yuanw-nix-home-macos.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "yuanwang-wf.cachix.org-1:P/RZ5Iuuuv2MYCNCnAsLfPGmgKMKeTwPaJclkrcwx80="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        "yuanw-nix-home-macos.cachix.org-1:6sDjrV0jQY6kRgXjXe0feuDtsxnoGDnkgvXuKma5JcQ="
      ];
      # https://github.com/NixOS/nix/issues/7273
      # auto-optimise-store = true;
      max-jobs = 12;
    };
    # Avoid unwanted garbage collection when using nix-direnv
    extraOptions = ''
      gc-keep-derivations = true
      gc-keep-outputs = true
      min-free = 17179870000
      max-free = 17179870000
      log-lines = 128

      experimental-features = nix-command flakes auto-allocate-uids
      keep-outputs          = true
      keep-derivations      = true
      fallback              = true
      extra-trusted-users   = ${config.my.username}
    '';
    # trustedBinaryCaches = config.nix.binaryCaches;
    gc = {
      # automatic = true;
      # interval = { Hour = 24 * 7; };
    };
  };
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = true;
    };
    overlays = [
      inputs.emacs.overlay
      inputs.nur.overlay
      inputs.firefox-darwin.overlay
      inputs.agenix.overlays.default
      inputs.emacs-lsp-booster.overlays.default
      (_final: _prev: {
        stable = inputs'.nixpkgs-stable.legacyPackages;

        # https://nixpk.gs/pr-tracker.html?pr=263500
        # https://gitlab.freedesktop.org/mesa/mesa/-/issues/8634
        # mesa = if _prev.stdenv.isDarwin then inputs.nixpkgs-stable.legacyPackages.${_prev.system}.mesa else
        #   # reiryoku-firmware =  inputs.reiryoku.packages.${prev.system}.firmware;
        #   # devenv = inputs.devenv.packages.${prev.system}.devenv;

        # use this variant if unfree packages are needed:
        # unstable = import nixpkgs-unstable {
        #   inherit system;
        #   config.allowUnfree = true;
        # };

      })
      (import ../packages)
    ];

  };
}
