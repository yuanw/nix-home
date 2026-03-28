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
        "https://cache.numtide.com"
      ];
      trusted-substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://yuanw-nix-home-macos.cachix.org"
        "https://cache.garnix.io"
        "https://cache.iog.io"
        "https://numtide.cachix.org"
        "https://cache.numtide.com"
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
        "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
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
      inputs.llm-agents.overlays.default
      inputs.agenix.overlays.default
      (
        _final: _prev:
        # mailutils 3.21 added nss_wrapper to nativeCheckInputs, which doesn't
        # build on darwin. NixOS/nixpkgs#503376 (merged to master as c207940c)
        # reverts this. Use nixpkgs-master's mailutils until nixos-unstable catches up.
        if _prev.stdenv.isDarwin then
          {
            mailutils = inputs'.nixpkgs-master.legacyPackages.mailutils;
          }
        else
          { }
      )
      (
        _final: _prev:
        # On macOS, the emacs-git pdmp (portable dump) captures the Nix build
        # sandbox path (/private/tmp/nix-build-.../etc) as data-directory.
        # When emacs is later used as a build tool for elisp packages, the new
        # sandbox returns EPERM (not ENOENT) for that stale temp path — causing
        # a fatal startup crash before EMACSDATA env var processing in startup.el.
        #
        # Fix: re-dump emacs immediately after install (while still inside the
        # build sandbox where those paths are accessible), with EMACSDATA and
        # native-comp-eln-load-path corrected to point at $out store paths.
        # The new pdmp captures the corrected values, eliminating the EPERM.
        if _prev.stdenv.isDarwin then
          {
            emacs-git =
              let
                emacs = _final.emacs-git;
                base = _prev.emacs-git.overrideAttrs (old: {
                  nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ _prev.makeWrapper ];
                  postInstall = (old.postInstall or "") + ''
                    emacs_version=$(ls "$out/share/emacs" | grep -E '^[0-9]' | sort -V | tail -1)
                    old_pdmp=$(find "$out/libexec/emacs/$emacs_version" -name 'emacs-*.pdmp' 2>/dev/null | head -1)
                    if [ -n "$old_pdmp" ]; then
                      tmp_pdmp="$old_pdmp.tmp"
                      eln_dir="$out/lib/emacs/$emacs_version/native-lisp"
                      EMACSDATA="$out/share/emacs/$emacs_version/etc" \
                      EMACSLOADPATH="$out/share/emacs/$emacs_version/lisp:" \
                        "$out/bin/emacs" \
                          --dump-file "$old_pdmp" \
                          --batch \
                          --no-site-file \
                          --eval "(progn (when (boundp 'native-comp-eln-load-path) (setq native-comp-eln-load-path (list \"$eln_dir/\"))) (dump-emacs-portable \"$tmp_pdmp\"))" \
                        && mv "$tmp_pdmp" "$old_pdmp" \
                        || echo "Warning: emacs re-dump failed, continuing with original pdmp"
                    fi
                    wrapProgram "$out/bin/emacs" \
                      --set-default EMACSDATA "$out/share/emacs/$emacs_version/etc"
                  '';
                });
              in
              base.overrideAttrs (oa: {
                passthru = oa.passthru // {
                  pkgs = oa.passthru.pkgs.overrideScope (_eself: _esuper: { inherit emacs; });
                };
              });
          }
        else
          { }
      )
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
        yt-dlp =
          if _prev.stdenv.isDarwin then inputs'.nixpkgs-stable.legacyPackages.yt-dlp else _prev.yt-dlp;

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
