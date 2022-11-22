{
  description = "Yuan Nix-darwin/NixOS Home";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devenv.url = "github:cachix/devenv/v0.2";
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
    emacs.url = "github:nix-community/emacs-overlay";
    resource-id.url = "github:yuanwang-wf/resource-id";
    ws-access-token.url = "github:yuanwang-wf/ws-access-token";
  };

  outputs = inputs@{ self, devenv, nixpkgs, darwin, home-manager, nur, emacs
    , resource-id, ws-access-token, devshell, flake-utils, ... }:
    let
      inherit (flake-utils.lib) eachDefaultSystem eachSystem;
      overlays = [
        emacs.overlay
        nur.overlay
        (final: prev: {
          devenv = inputs.devenv.defaultPackage.${prev.system};
          resource-id = inputs.resource-id.defaultPackage.${prev.system};
          ws-access-token =
            inputs.ws-access-token.defaultPackage.${prev.system};
        })
        (import ./overlays)
      ];

      # idea borrowed from https://github.com/hardselius/dotfiles
      mkDarwinSystem = { modules }:
        darwin.lib.darwinSystem {
          inputs = inputs;
          system = "x86_64-darwin";
          modules = [
            { nixpkgs.overlays = overlays; }

            ({ lib, ... }: {
              imports = import ./modules/modules.nix {
                inherit lib;
                isDarwin = true;
              };
            })
            home-manager.darwinModules.home-manager
            ./macintosh.nix
          ] ++ modules;
        };
      mkNixSystem = { modules }:
        nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = modules ++ [
            ./nixos_system.nix
            home-manager.nixosModules.home-manager
            {

              nixpkgs.overlays = overlays;
            }
            ({ lib, pkgs, ... }: {
              imports = import ./modules/modules.nix {
                inherit lib;
                isNixOS = true;
              };
            })
          ];
        };
    in {
      nixosConfigurations.asche = mkNixSystem {
        modules = [ ./machines/asche/configuration.nix ./hosts/asche.nix ];
      };

      darwinConfigurations = {
        yuanw = mkDarwinSystem { modules = [ ./hosts/yuan-mac.nix ]; };
        wf17084 = mkDarwinSystem { modules = [ ./hosts/wf17084.nix ]; };
      };
      yuanw = self.darwinConfigurations.yuanw.system;
      wf17084 = self.darwinConfigurations.wf17084.system;

    } // eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ devshell.overlay ];
        };

        # myHaskellEnv = (pkgs.haskellPackages.ghcWithHoogle
        #   (p: with p; [ cabal-install ormolu hlint hpack brittany turtle ]));

      in {
        devShell = pkgs.devshell.mkShell {
          name = "nix-home";
          imports = [ (pkgs.devshell.extraModulesDir + "/git/hooks.nix") ];
          git.hooks.enable = true;
          git.hooks.pre-commit.text = "${pkgs.treefmt}/bin/treefmt";
          packages = [
            pkgs.ormolu
            # pkgs.haskellPackages.hnix
            pkgs.treefmt
            pkgs.nixfmt
          ];
        };
      });
}
