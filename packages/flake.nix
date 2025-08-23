{
  description = "My packages";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
      ];
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem =
        { config, pkgs, ... }:
        {
          overlayAttrs = config.packages;
          packages = rec {
            sketchybar-cpu-helper = pkgs.callPackage ./sketchybar-cpu-helper { };
            bandcamp-dl = pkgs.python3Packages.callPackage ./bandcamp { };
            choose-mac = pkgs.callPackage ./choose-mac.nix { };
            sf-symbols = pkgs.callPackage ./sf_symbols.nix { };
            font-hack-nerd-font = pkgs.callPackage ./font-hack-nerd-font.nix { };
            # https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/emacs-packages.nix
            hurl-mode = pkgs.callPackage ./hurl-mode.nix {
              melpaBuild = pkgs.stdenv.mkDerivation;
              inherit (pkgs) fetchFromGitHub writeText;
            };

          };
        };
      flake = {
      };
    };
}
