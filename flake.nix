{
  description = "Yuan Nix-darwin flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-20.09-darwin";
    #nixpkgs.url = "github:nixos/nixpkgs";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-20.09";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    emacs.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, darwin, home-manager, nur, emacs }:
    let
      fullName = "Yuan Wang";
      #pkgs = import nixpkgs { system = "x86_64-darwin"; };
    in {
      darwinConfigurations."yuan-mac" = darwin.lib.darwinSystem {
        modules =
          [ home-manager.darwinModules.home-manager ./configuration.nix ];
        inputs = { inherit darwin nixpkgs home-manager; };
      };
    };
}
