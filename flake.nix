{
  description = "Yuan Nix-darwin/NixOS flake";

  inputs = {
    #nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-20.09-darwin";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # https://github.com/LnL7/nix-darwin/pull/308/files
    darwin.url = "github:LnL7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    #home-manager.url = "github:nix-community/home-manager/release-20.09";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    kmonad = {
      url = "github:david-janssen/kmonad";
      flake = false;
    };
    emacs.url = "github:nix-community/emacs-overlay";
  };

  outputs =
    inputs@{ self, nixpkgs, darwin, home-manager, nur, emacs, kmonad, ... }:
    let
      # copied from https://github.com/cmacrae/config
      mailAddr = name: domain: "${name}@${domain}";
      # idea borrowed from https://github.com/hardselius/dotfiles
      mkDarwinSystem = { localConfig, modules }:
        darwin.lib.darwinSystem {
          inputs = { inherit darwin nixpkgs emacs nur home-manager; };
          modules = modules ++ [
            ({ lib, ... }: {
              _module.args.localConfig = localConfig;
              imports = import ./modules/modules.nix {
                inherit lib;
                isDarwin = true;
              };
            })
            home-manager.darwinModules.home-manager
            ./system.nix
          ];
        };
      mkNixSystem = { localConfig, modules }:
        nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = modules ++ [
            ./nixos_system.nix
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
              _module.args.localConfig = localConfig;
              imports = import ./modules/modules.nix { inherit lib; };
            })
          ];
        };
    in {
      nixosConfigurations.nixos = mkNixSystem {
        localConfig = {
          username = "yuanwang";
          name = "Yuan Wang";
          email = mailAddr "me" "yuanwang.ca";
          hostname = "yuan-mac";
          gpgKey = "BF2ADAA2A98F45E7";
          homeDirectory = "/home/yuanwang";
        };
        modules = [
          ({ config, pkgs, localConfig, ... }: {
            home-manager.users.${localConfig.username} = {
              xdg.enable = true;
              home.file = {
                ".xmobarrc".text = ''
                  Config { font = "xft:PragmataPro:size=18:bold"
                          , borderColor = "black"
                          , border = TopB
                          , bgColor = "black"
                          , fgColor = "grey"
                          , allDesktops = True
                          , position = TopW L 100
                          , commands = [ Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                                          -- Cpu usage in percent
                                          , Run Cpu ["-t", "<fn=1>\xf108</fn>  cpu: (<total>%)","-H","50","--high","red"] 20
                                          -- Ram used number and percent
                                          , Run Memory ["-t", "<fn=1>\xf233</fn>  mem: <used>M (<usedratio>%)"] 20
                                          -- Disk space free
                                          , Run DiskU [("/", "<fn=1>\xf0c7</fn>  hdd: <free> free")] [] 60
                                          , Run Swap [] 10
                                          , Run Wireless "wlp0s20f3" [ ] 20
                                          , Run DynNetwork [] 10
                                          , Run Com "uname" ["-s","-r"] "" 36000
                                          , Run Date "<fn=1>\xf133</fn> %a %b %_d %Y %H:%M:%S" "date" 10
                                          , Run Battery ["-t", "<acstatus>: <left>% - <timeleft>",
                  	                               "--",--"-c", "charge_full"
                                                         "-O", "AC",
                                                         "-o", "Bat",
                                                         "-h", "green",
                                                         "-l", "red"] 10
                                          ]
                          , sepChar = "%"
                          , alignSep = "}{"
                          , template = " %battery% | %cpu% | %memory% * %swap% | %disku% | %date% | %uname% | %wlp0s20f3wi% %dynnetwork% "
                          }

                '';
                ".config/pass-git-helper/git-pass-mapping.ini".text = ''
                  [github.com*]
                  target=github
                '';
              };
              services.blueman-applet.enable = true;
              services.network-manager-applet.enable = true;
              services.dunst.enable = true;
              xsession = {
                enable = true;
                windowManager.xmonad = {
                  enable = true;
                  enableContribAndExtras = true;
                  config = ./xmonad/xmonad.hs;
                };
              };
              programs = {
                password-store = { enable = true; };
                rofi = {
                  enable = true;
                  terminal = "${pkgs.alacritty}/bin/alaritty";
                  theme = ./modules/theme.rafi;
                };
                git.extraConfig = {
                  github.user = "yuanw";
                  credential.helper =
                    "${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper";
                };
              };
            };
            programs = {
              editors.emacs = {
                enable = true;
                pkg = pkgs.emacsPgtkGcc;
              };
            };
          })
        ];
      };
      darwinConfigurations = {
        "yuan-mac" = mkDarwinSystem {
          localConfig = {
            username = "yuanwang";
            name = "Yuan Wang";
            email = mailAddr "me" "yuanwang.ca";
            hostname = "yuan-mac";
            gpgKey = "BF2ADAA2A98F45E7";
            homeDirectory = "/Users/yuanwang";
          };
          modules = [
            ({ lib, pkgs, config, localConfig, ... }: {
              home-manager.users.${localConfig.username}.programs.git = {
                extraConfig = { github.user = "yuanw"; };
              };
              modules.wm.yabai.enable = true;
              programs = {
                node.enable = true;
                python.enable = true;
                haskell.enable = true;
                editors.emacs.enable = true;
                stevenBlackHosts.enable = true;
              };
            })
          ];
        };

        "wf17084" = mkDarwinSystem {
          localConfig = {
            username = "yuanwang";
            name = "Yuan Wang";
            email = mailAddr "yuan.wang" "workiva.com";
            hostname = "wf17084";
            gpgKey = "19AD3F6B1A5BF3BF";
            homeDirectory = "/Users/yuanwang";
          };
          modules = [
            ({ lib, pkgs, config, localConfig, ... }: {
              home-manager.users.${localConfig.username}.programs.git = {
                extraConfig = { github.user = "yuanwang-wf"; };
              };
              modules.wm.yabai.enable = true;
              programs = {
                editors.emacs.enable = true;
                dart.enable = true;
                workShell.enable = true;
              };
            })
          ];
        };

      };
    };
}
