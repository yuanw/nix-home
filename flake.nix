{
  description = "Yuan Nix-darwin/NixOS Home";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
    emacs.url = "github:nix-community/emacs-overlay";
    mac-emacs.url = "github:cmacrae/emacs";
    resource-id.url = "github:yuanwang-wf/resource-id";
    ws-access-token.url = "github:yuanwang-wf/ws-access-token";
    sops-nix.url = "github:Mic92/sops-nix";
    nix-script = {
      url = "github:BrianHicks/nix-script";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agda = {
      url = "github:agda/agda?rev=4dee747be7aa6cf3947213862e1da9b710b2e40b";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = inputs@{ self, nixpkgs, darwin, home-manager, nur, emacs, mac-emacs
    , resource-id, ws-access-token, devshell, flake-utils, nix-script, sops-nix
    , agda, ... }:
    let
      inherit (flake-utils.lib) eachDefaultSystem eachSystem;
      # idea borrowed from https://github.com/hardselius/dotfiles
      mkDarwinSystem = { modules }:
        darwin.lib.darwinSystem {
          inputs = inputs;
          system = "x86_64-darwin";
          modules = [
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
            sops-nix.nixosModules.sops
            home-manager.nixosModules.home-manager
            {
              nixpkgs.overlays = [
                (se: su: { myInputs = inputs; })
                emacs.overlay
                nur.overlay
                (import ./overlays)
              ];
            }
            ({ lib, pkgs, ... }: {
              imports = import ./modules/modules.nix { inherit lib; };
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
          overlays = [ devshell.overlay nix-script.overlay ];
        };

        myHaskellEnv = (pkgs.haskellPackages.ghcWithHoogle
          (p: with p; [ cabal-install ormolu hlint hpack brittany turtle ]));

      in {
        devShell = pkgs.devshell.mkShell {
          name = "nix-home";
          imports = [ (pkgs.devshell.extraModulesDir + "/git/hooks.nix") ];
          git.hooks.enable = true;
          git.hooks.pre-commit.text = "${pkgs.treefmt}/bin/treefmt";
          packages = [
            myHaskellEnv
            pkgs.haskellPackages.hnix
            pkgs.treefmt
            pkgs.nixfmt
            pkgs.nix-script
            pkgs.nix-script-haskell
            pkgs.nix-script-bash
          ];
        };
      });
}
