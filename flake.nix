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
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    nix-doom-emacs.inputs.nixpkgs.follows = "nixpkgs";
    nix-doom-emacs.inputs.emacs-overlay.follows = "emacs";
    my.url = "path:./my";
  };

  outputs = { self, nixpkgs, darwin, home-manager, nur, emacs, kmonad, my
    , nix-doom-emacs }:
    let mailAddr = name: domain: "${name}@${domain}";
    in {
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
                python.enable = true;
                editors.emacs.enable = true;
              };
            })
          ];
          inputs = {
            inherit darwin nixpkgs emacs nur home-manager nix-doom-emacs;
          };
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
                python.enable = true;
                editors.emacs.enable = true;
                dart.enable = true;
                workShell.enable = true;
              };
            })
          ];
          inputs = { inherit darwin nixpkgs emacs nur home-manager; };
        };

      };
    };
}
