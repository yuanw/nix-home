{
  inputs,
  inputs',
  config,
  ...
}:
let
  # Nix and Lix use different names for the same pipe-operator parser feature.
  pipeOperatorFeature =
    if builtins.elem config.my.hostname [ "mist" ] then "pipe-operator" else "pipe-operators";
in
{
  nix = {
    # configureBuildUsers = true;
    settings = {
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

      experimental-features = nix-command flakes auto-allocate-uids ${pipeOperatorFeature}
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
    overlays =

      [
        inputs.emacs.overlay
        inputs.nima.overlays.default
        inputs.nur.overlays.default
        inputs.mcp-servers-nix.overlays.default
        inputs.llm-agents.overlays.shared-nixpkgs
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
          yt-dlp =
            if _prev.stdenv.hostPlatform.isDarwin then
              inputs'.nixpkgs-stable.legacyPackages.yt-dlp
            else
              _prev.yt-dlp;

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

          # buildPythonPackage maps `doCheck` → underlying `doInstallCheck`; overrideAttrs on
          # `doCheck` does not skip pytest. Use overridePythonAttrs (all hosts; mcp-atlassian + HM MCP).
          python313Packages = _prev.python313Packages.overrideScope (
            _pyfinal: pyprev: {
              fastmcp = pyprev.fastmcp.overridePythonAttrs (_: {
                doCheck = false;
              });
            }
          );
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
