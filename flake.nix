{
  description = "Yuan Nix-darwin/NixOS Home";

  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devenv.url = "github:cachix/devenv";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    nix-colors.url = "github:misterio77/nix-colors";
    hosts.url = "github:StevenBlack/hosts";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    astro-nvim = {
      url = "github:AstroNvim/AstroNvim";
      flake = false;
    };
    haskell-flake.url = "github:srid/haskell-flake";
    nur.url = "github:nix-community/NUR";
    emacs.url = "github:nix-community/emacs-overlay";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.darwin.follows = "nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };


  outputs = inputs @ { flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      imports = [
        ./devshell.nix
        ./hosts
        ./modules
        inputs.pre-commit.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { system, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          # overlays = [
          #   (_final: _prev: {
          #     # https://gitlab.freedesktop.org/mesa/mesa/-/issues/8634
          #     mesa = if _prev.stdenv.isDarwin then inputs.nixpkgs-stable.legacyPackages.${_prev.system}.mesa else
          #     inputs.nixpkgs.legacyPackages.${_prev.system}.mesa;
          #   })
          # ];
          config = {
            allowUnfree = true;
            # allowUnsupportedSystem = true;
          };
        };
        haskellProjects.default = {
          projectRoot = ./packages;
          settings = { };
          # overrides = self: super: { };
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
          devShell = {
            hlsCheck.enable = false;
          };
        };

        treefmt.imports = [ ./treefmt.nix ];
        # https://github.com/cachix/pre-commit-hooks.nix/blame/30d1c34bdbfe3dd0b8fbdde3962180c56cf16f12/flake-module.nix
        pre-commit.settings.hooks.treefmt.enable = true;

      };

    };
}
