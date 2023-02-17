{
  description = "Yuan Nix-darwin/NixOS Home";

  # nixConfig = {
  #   extra-trusted-public-keys =
  #     "yuanw-nix-home-macos.cachix.org-1:6sDjrV0jQY6kRgXjXe0feuDtsxnoGDnkgvXuKma5JcQ=";
  #   extra-substituters = "https://yuanw-nix-home-macos.cachix.org";
  # };
  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.11";
    nixpkgs.url = "nixpkgs/nixos-unstable";
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hosts.url = "github:StevenBlack/hosts";
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

  outputs = inputs@{ self, nixpkgs-stable, devenv, nixpkgs, darwin, home-manager
    , nur, emacs, resource-id, ws-access-token, devshell, flake-utils, hosts
    , ... }:
    let
      inherit (flake-utils.lib) eachDefaultSystem eachSystem;
      overlays = [
        emacs.overlay
        nur.overlay
        (final: prev: {
          stable = nixpkgs-stable.legacyPackages.${prev.system};
          # use this variant if unfree packages are needed:
          # unstable = import nixpkgs-unstable {
          #   inherit system;
          #   config.allowUnfree = true;
          # };

        })
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
            hosts.nixosModule
            {
              networking.stevenBlackHosts = {
                enable = true;
                blockFakenews = true;
                blockGambling = true;
                blockPorn = true;
                blockSocial = false;
              };
            }
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

      asche = self.nixosConfigurations.asche.system;
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
            # pkgs.ormolu
            # pkgs.haskellPackages.hnix
            pkgs.treefmt
            pkgs.nixfmt
          ];
        };
      });
}
