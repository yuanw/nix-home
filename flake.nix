{
  description = "Yuan Nix-darwin/NixOS Home";

  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    # https://github.com/NixOS/nixpkgs/pull/257760
    ollama-nixpkgs.url = "github:elohmeier/nixpkgs/ollama";
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
    emacs-plus = {
      url = "github:d12frosted/homebrew-emacs-plus";
      flake = false;
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.darwin.follows = "nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agda = {
      url = "github:agda/agda";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Firefox Darwin overlay.
    firefox-darwin = {
      url = "github:bandithedoge/nixpkgs-firefox-darwin";
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
          overlays = [
            (_self: super: {
              # Stork is marked as broken on intel mac, but it does work.
              # Unfortunately we cannot test this code PATH due to lack of CI for intel mac (#335).
              haskellPackages.monomer = if system == "x86_64-darwin" then super.haskellPackages.monomer.overrideAttrs (_oa: { meta.broken = false; }) else super.haskellPackages.monomer;
            })
          ];
          config = {
            allowUnfree = true;
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
