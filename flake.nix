{
  description = "Yuan Nix-darwin/NixOS flake";

  inputs = {
    #nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-20.09-darwin";
    nixpkgs.url = "github:nixos/nixpkgs";
    # https://github.com/LnL7/nix-darwin/pull/308/files
    darwin.url = "github:hardselius/nix-darwin/master";
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
  };

  outputs =
    inputs@{ self, nixpkgs, darwin, home-manager, nur, emacs, kmonad, my, ... }:
    let mailAddr = name: domain: "${name}@${domain}";
    in {
      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            my.my
            {
              my.username = "yuanwang";
              my.name = "Yuan Wang";
              my.email = mailAddr "me" "yuanwang.ca";
              my.hostname = "nixos";
              my.gpgKey = "BF2ADAA2A98F45E7";
              my.homeDirectory = "/home/yuanwang";
            }
            ./nixos_system.nix
            home-manager.nixosModules.home-manager
          ];
        };
      darwinConfigurations = {
        "yuan-mac" = darwin.lib.darwinSystem {
          modules = [
            my.my
            {
              my.username = "yuanwang";
              my.name = "Yuan Wang";
              my.email = mailAddr "me" "yuanwang.ca";
              my.hostname = "yuan-mac";
              my.gpgKey = "BF2ADAA2A98F45E7";
              my.homeDirectory = "/Users/yuanwang";
            }
            ./system.nix
            home-manager.darwinModules.home-manager
            ({ lib, pkgs, config, ... }: {
              home-manager.users.${config.my.username}.programs.git = {
                extraConfig = { github.user = "yuanw"; };
              };
              programs = {
                node.enable = true;
                python.enable = true;
                haskell.enable = true;
                editors.emacs.enable = true;
                stevenBlackHosts.enable = true;
                wm.enable = true;
              };
            })
          ];
          inputs = { inherit darwin nixpkgs emacs nur home-manager; };
        };

        "wf17084" = darwin.lib.darwinSystem {
          modules = [
            my.my
            {
              my.username = "yuanwang";
              my.name = "Yuan Wang";
              my.email = mailAddr "yuan.wang" "workiva.com";
              my.hostname = "wf17084";
              my.gpgKey = "19AD3F6B1A5BF3BF";
              my.homeDirectory = "/Users/yuanwang";
            }
            ./system.nix
            home-manager.darwinModules.home-manager
            ({ lib, pkgs, config, ... }: {
              home-manager.users.${config.my.username}.programs.git = {
                extraConfig = { github.user = "yuanwang-wf"; };
              };
              programs = {
                editors.emacs.enable = true;
                dart.enable = true;
                workShell.enable = true;
                wm.enable = true;
              };
            })
          ];
          inputs = { inherit darwin nixpkgs emacs nur home-manager; };
        };

      };
    };
}
