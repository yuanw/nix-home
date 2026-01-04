{
  inputs,
  inputs',
  config,
  ...
}:
{
  nix = {
    # configureBuildUsers = true;
    settings = {
      trusted-users = [
        "root"
        config.my.username
      ];
      substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://yuanw-nix-home-macos.cachix.org"
        "https://cachix.org/api/v1/cache/yuanwang-wf"
        "https://cachix.org/api/v1/cache/devenv"
        "https://cache.garnix.io"
        "https://cache.iog.io"
        "https://cache.zw3rk.com"
        "https://numtide.cachix.org"
      ];
      trusted-substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://yuanw-nix-home-macos.cachix.org"
        "https://cache.garnix.io"
        "https://cache.iog.io"
        "https://numtide.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "yuanwang-wf.cachix.org-1:P/RZ5Iuuuv2MYCNCnAsLfPGmgKMKeTwPaJclkrcwx80="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        "yuanw-nix-home-macos.cachix.org-1:6sDjrV0jQY6kRgXjXe0feuDtsxnoGDnkgvXuKma5JcQ="
        "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
        "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ik6cQjb41X0iXVXeHigGmycPPE="
      ];
      # https://github.com/NixOS/nix/issues/7273
      # auto-optimise-store = true;
      max-jobs = 12;
      download-buffer-size = 1000000000; # 1G

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
      inputs.nur.overlays.default
      inputs.mcp-servers-nix.overlays.default
      inputs.agenix.overlays.default
      (_final: _prev: {
        stable = inputs'.nixpkgs-stable.legacyPackages;
        # gtk3 =
        #   if _prev.stdenv.isDarwin then
        #     inputs'.nixpkgs-stable.legacyPackages.gtk3
        #   else
        #     inputs.nixpkgs.legacyPackages.${_prev.system}.gtk3;
        # sbcl =
        #   if _prev.stdenv.isDarwin then
        #     inputs'.nixpkgs-stable.legacyPackages.sbcl
        #   else
        #     inputs.nixpkgs.legacyPackages.${_prev.system}.sbcl;
        sioyek = inputs'.nixpkgs-stable.legacyPackages.sioyek;
        # batgrep =
        #   if _prev.stdenv.isDarwin then
        #     _prev.batgrep.overrideAttrs (_oldAttrs: {
        #       doCheck = false;
        #     })
        #   else
        #     _prev.batgrep;
        #https://github.com/NixOS/nixpkgs/pull/476210
        trayer =
          if !_prev.stdenv.isDarwin then inputs'.nixpkgs-master.legacyPackages.trayer else _prev.trayer;
        pasystray =
          if !_prev.stdenv.isDarwin then inputs'.nixpkgs-master.legacyPackages.pasystray else _prev.pasystray;

        #https://github.com/NixOS/nixpkgs/pull/476003/files
        #pasystray = inputs'.nixpkgs-master.legacyPackages.pasystray;
        # Override go-jira to use current master
        go-jira = _prev.go-jira.overrideAttrs (_oldAttrs: {
          version = "unstable-2025-11-27";
          src = _prev.fetchFromGitHub {
            owner = "go-jira";
            repo = "jira";
            rev = "748b7d552f8b3ad993b05810b93f0f2ed39822d1";
            hash = "sha256-PFmgnGGayrgcC46UvvSzCQ1uVc87H1kgWBdMrcCRZD4=";
          };
        });

        # Override jiratui to use current master
        jiratui = _prev.jiratui.overrideAttrs (_oldAttrs: {
          version = "unstable-2025-11-27";
          src = _prev.fetchFromGitHub {
            owner = "whyisdifficult";
            repo = "jiratui";
            rev = "fc97e1d8e81c6a3fb8537eb60b176a5ad1b73392";
            hash = "sha256-Otds9VFEgDvlOhSj+tWL/34/T1Q9tWU3BNbfCrxBiy4=";
          };
        });
        #gjs = inputs'.nixpkgs-stable.legacyPackages.gjs;

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
