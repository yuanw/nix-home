{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";

  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          # packages.resource-id.root =
          #   ./resource-id; # This value is detected based on .cabal files
          # packages.ws-access-token.root =
          #   ./ws-access-token; # This value is detected based on .cabal files
          # packages.hi-chew.root = ./hi-chew;
          # packages.mono-stretchly.root = ./mono-stretchly;
          settings = {

          };
          # overrides = self: super: { };
          devShell = {
            # enable = true; # Enabled by default
            tools = hp:
              {
                treefmt = config.treefmt.build.wrapper;
              } // config.treefmt.build.programs;
            hlsCheck.enable = false;
          };
        };
        # Auto formatters. This also adds a flake check to ensure that the
        # source tree was auto formatted.
        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [ "--ghc-opt" "-XImportQualifiedPost" ];
          };
        };
        # haskell-flake doesn't set the default package, but you can do it here.
        packages.resource-id = self'.packages.resource-id;
        packages.mono-stretchly = self'.packages.mono-stretchly;
        packages.ws-access-token = self'.packages.ws-access-token;
        packages.hi-chew = self'.packages.hi-chew;
        packages.default = self'.packages.mono-stretchly;
        apps.default = self'.apps.mono-stretchly;
        # Dev shell scripts.
        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            exec = ''
              echo http://127.0.0.1:8888
              hoogle serve -p 8888 --local
            '';
            category = "Dev Tools";
          };
          repl = {
            description = "Start the cabal repl";
            exec = ''
              cabal repl "$@"
            '';
            category = "Dev Tools";
          };
          fmt = {
            description = "Format the source tree";
            exec = "${lib.getExe config.treefmt.build.wrapper}";
            category = "Dev Tools ";
          };
          run = {
            description = "Run the project with ghcid auto-recompile";
            exec = ''
              ghcid -c "cabal repl exe:hi-chew" --warnings -T :main
            '';
            category = "Primary";
          };
        };

        # Default shell.
        # devShells.default = pkgs.mkShell {
        #   inputsFrom = [ config.mission-control.devShell
        #                  self'.devShells.default ];
        # };
      };
    };
}
