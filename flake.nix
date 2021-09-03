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
    mac-emacs.url = "github:cmacrae/emacs";
    spacebar.url = "github:cmacrae/spacebar";
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    resource-id.url = "github:yuanwang-wf/resource-id";
    ws-access-token.url = "github:yuanwang-wf/ws-access-token";
    # https://github.com/cmacrae/spacebar/blob/master/flake.nix#L4
    # spacebar.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, darwin, home-manager, nur, emacs, kmonad
    , spacebar, mac-emacs, resource-id, ws-access-token, ... }:
    let
      # copied from https://github.com/cmacrae/config
      mailAddr = name: domain: "${name}@${domain}";
      # idea borrowed from https://github.com/hardselius/dotfiles
      mkDarwinSystem = { localConfig, modules }:
        darwin.lib.darwinSystem {
          inputs = {
            inherit darwin nixpkgs emacs nur home-manager spacebar mac-emacs
              resource-id ws-access-token;
          };
          modules = modules ++ [
            ({ lib, ... }: {
              _module.args.localConfig = localConfig;
              imports = import ./modules/modules.nix {
                inherit lib;
                isDarwin = true;
              };
            })
            home-manager.darwinModules.home-manager
            ./macintosh.nix
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
      nixosConfigurations.asche = mkNixSystem {
        localConfig = {
          username = "yuanwang";
          name = "Yuan Wang";
          email = mailAddr "me" "yuanwang.ca";
          hostname = "asche";
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
                autorandr = {
                  enable = true;
                  profiles = {
                    home = {
                      fingerprint = {
                        DP-1 =
                          "00ffffffffffff001e6d0777aba10400091d0104b53c22789e3e31ae5047ac270c50542108007140818081c0a9c0d1c08100010101014dd000a0f0703e803020650c58542100001a286800a0f0703e800890650c58542100001a000000fd00383d1e8738000a202020202020000000fc004c472048445220344b0a20202001160203197144900403012309070783010000e305c000e3060501023a801871382d40582c450058542100001e565e00a0a0a029503020350058542100001a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000029";
                        eDP-1 =
                          "00ffffffffffff004d10cc14000000002d1d0104b51d12780e6e60a95249a1260d50540000000101010101010101010101010101010172e700a0f06045903020360020b41000001828b900a0f06045903020360020b410000018000000fe004b384a3057804c513133345231000000000002410332011200000b010a2020014202030f00e3058000e606050160602800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000aa";
                      };
                      config = {
                        eDP-1 = {
                          enable = true;
                          crtc = 0;
                          primary = true;
                          position = "0x0";
                          mode = "3840x2400";
                          rate = "59.99";
                        };
                        DP-1 = {
                          enable = true;
                          crtc = 1;
                          mode = "3840x2160";
                          position = "3840x0";
                          rate = "60.00";
                        };
                      };
                    };
                  };
                };
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
            modules = {
              terminal.enable = true;
              browsers.firefox.enable = true;
            };
            programs = {
              haskell.enable = true;
              python.enable = true;
              editors.emacs = {
                enable = true;
                pkg = pkgs.emacsPgtkGcc;
              };
            };
          })
        ];
      };
      darwinConfigurations = {
        yuan-mac = mkDarwinSystem {
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
              modules = {
                terminal.enable = true;
                wm.yabai.enable = true;
                brew = {
                  enable = true;
                  casks = [ "firefox" "racket" ];
                };
              };
              programs = {
                node.enable = true;
                python.enable = true;
                haskell.enable = true;
                editors.emacs = {
                  enable = true;
                  pkg = pkgs.emacsMacport;
                };
                stevenBlackHosts.enable = true;
              };
            })
          ];
        };

        wf17084 = mkDarwinSystem {
          localConfig = {
            username = "yuanwang";
            name = "Yuan Wang";
            email = mailAddr "yuan.wang" "workiva.com";
            hostname = "wf17084";
            gpgKey = "19AD3F6B1A5BF3BF";
            homeDirectory = "/Users/yuanwang";
          };
          modules = [
            ({ lib, pkgs, config, localConfig, services, ... }: {
              home-manager.users.${localConfig.username}.programs.git = {
                extraConfig = { github.user = "yuanwang-wf"; };
              };
              modules = {
                brew = {
                  enable = true;
                  casks = [ "firefox" "racket" ];
                  brews =
                    [ "aws-iam-authenticator" "reattach-to-user-namespace" ];
                };
                # browsers.firefox = {
                #   enable = true;
                #   pkg = pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";
                # };
                dev = { julia.enable = true; };
                terminal.enable = true;
                wm.yabai.enable = true;
              };
              programs = {
                node.enable = true;
                editors.emacs = {
                  enable = true;
                  pkg = pkgs.emacs;
                };
                python.enable = true;
                haskell.enable = true;
                dart.enable = true;
                workShell.enable = true;
              };
            })
          ];
        };

      };
    };
}
