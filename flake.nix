{
  description = "Yuan Nix-darwin/NixOS Home";

  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.11";
    nixpkgs.url = "nixpkgs/nixos-unstable";
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-colors.url = "github:misterio77/nix-colors";
    hosts.url = "github:StevenBlack/hosts";
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
    emacs.url = "github:nix-community/emacs-overlay";
    reiryoku.url = "github:yuanw/reiryoku";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.darwin.follows = "darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs-stable, nixpkgs, darwin, home-manager, nur
    , emacs, devshell, flake-utils, hosts, reiryoku, agenix, nix-colors, ... }:
    let
      inherit (flake-utils.lib) eachDefaultSystem eachSystem;
      overlays = [
        emacs.overlay
        nur.overlay
        agenix.overlays.default
        (final: prev: {
          stable = nixpkgs-stable.legacyPackages.${prev.system};
          # use this variant if unfree packages are needed:
          # unstable = import nixpkgs-unstable {
          #   inherit system;
          #   config.allowUnfree = true;
          # };

        })
        (final: prev: {
          reiryoku-firmware = inputs.reiryoku.packages.${prev.system}.firmware;
        })
        (import ./hs-land/overlay.nix)
        (import ./overlays)
      ];

      # idea borrowed from https://github.com/hardselius/dotfiles
      mkDarwinSystem = { modules }:
        darwin.lib.darwinSystem {
          inputs =  {
            inherit nix-colors;
            isNixOS = false;
            isDarwin = true;
          };

          system = "x86_64-darwin";
          modules = [
            { nixpkgs.overlays = overlays; }
            ({ lib, ... }: {
              imports = import ./modules/modules.nix {
                inherit lib;
                isDarwin = true;
              };
            })

            agenix.darwinModules.age
            home-manager.darwinModules.home-manager
             nix-colors.homeManagerModule
            ./macintosh.nix
          ] ++ modules;
        };
      mkNixSystem = { modules }:
        nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = inputs // {
            isNixOS = true;
            isDarwin = false;
          };
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

            agenix.nixosModules.age
            home-manager.nixosModules.home-manager
            { nixpkgs.overlays = overlays; }
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
      let pkgs = import nixpkgs { inherit system; };
      in {
        # devShell = pkgs.devshell.mkShell {
        #   name = "nix-home";
        #   imports = [ (pkgs.devshell.extraModulesDir + "/git/hooks.nix") ];
        #   git.hooks.enable = true;
        #   git.hooks.pre-commit.text = "${pkgs.treefmt}/bin/treefmt";
        #   packages = [ pkgs.treefmt pkgs.nixfmt ];
        # };
      });
}
