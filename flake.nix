{
  description = "Try out flake";

  inputs = {
    pkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-20.09-darwin";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "pkgs-unstable";
  };

  outputs = inputs @ { self, nixpkgs,home-manager,  darwin }: {

   darwinConfigurations."yuan-mac" = darwin.lib.darwinSystem {
     modules = [
       inputs.home-manager.darwinModules.home-manager
       ./machines/yuan-mac/configuration.nix

         ({ config, home-manager, pkgs, secrets, ... }: {

              imports = [
                (import ./machines/yuan-mac/configuration.nix {
                  inherit home-manager pkgs;
                })
              ];
          })
     ];
    };

  };
}
