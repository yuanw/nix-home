{
  description = "Yuan Nix-darwin flake";

  inputs = {
    #nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-20.09-darwin";
    nixpkgs.url = "github:nixos/nixpkgs";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    #home-manager.url = "github:nix-community/home-manager/release-20.09";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    kmonad = {
      url = "github:david-janssen/kmonad";
      flake = false;
    };
    emacs.url = "github:nix-community/emacs-overlay";
    my.url = "path:./my";
    #my.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, darwin, home-manager, nur, emacs, kmonad, my }:
    let mailAddr = name: domain: "${name}@${domain}";
    in {
      darwinConfigurations = {
        "yuan-mac" = darwin.lib.darwinSystem {
          modules = [
            my
            {
              my.username = "yuanwang";
              my.name = "Yuan Wang";
              my.email = mailAddr "me" "yuanwang.ca";
              my.hostname = "yuan-mac";
              my.gpgKey = "BF2ADAA2A98F45E7";
            }
            ./configuration.nix
            home-manager.darwinModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = false;
              home-manager.users.yuanwang = import ./home.nix;
            }
          ];
          inputs = { inherit darwin nixpkgs emacs nur home-manager; };
        };
      };
    };
}
