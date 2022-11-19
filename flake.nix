{
  description = "Yuan Nix-darwin/NixOS Home";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devenv.url = "github:cachix/devenv/v0.2";
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
    emacs.url = "github:nix-community/emacs-overlay";
    # mac-emacs.url = "github:cmacrae/emacs";
    resource-id.url = "github:yuanwang-wf/resource-id";
    ws-access-token.url = "github:yuanwang-wf/ws-access-token";
  };

  outputs = inputs@{ self, devenv, nixpkgs, darwin, home-manager, nur, emacs
    , resource-id, ws-access-token, devshell, flake-utils, ... }:
    let
      inherit (flake-utils.lib) eachDefaultSystem eachSystem;
      # idea borrowed from https://github.com/hardselius/dotfiles
      mkDarwinSystem = { modules }:
        darwin.lib.darwinSystem {
          inputs = inputs;
          system = "x86_64-darwin";
          modules = [
            {
              nix = {
                configureBuildUsers = true;
                settings = {
                  substituters = [
                    "https://utdemir.cachix.org"
                    "https://hs-nix-template.cachix.org"
                    "https://cache.nixos.org"
                    "https://nix-community.cachix.org"
                    "https://cachix.org/api/v1/cache/yuanwang-wf"
                    "https://cachix.org/api/v1/cache/emacs"
                    "https://cachix.org/api/v1/cache/devenv"
                  ];
                  trusted-public-keys = [
                    "utdemir.cachix.org-1:mDgucWXufo3UuSymLuQumqOq1bNeclnnIEkD4fFMhsw="
                    "hs-nix-template.cachix.org-1:/YbjZCrYAw7d9ayLayk7ZhBdTEkR10ZFmFuOq6ZJo4c="
                    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
                    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
                    "yuanwang-wf.cachix.org-1:P/RZ5Iuuuv2MYCNCnAsLfPGmgKMKeTwPaJclkrcwx80="
                    "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
                    "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
                  ];

                  max-jobs = 8;
                };
                # Avoid unwanted garbage collection when using nix-direnv
                extraOptions = ''
                  experimental-features = nix-command flakes
                  keep-outputs          = true
                  keep-derivations      = true
                '';
                # trustedBinaryCaches = config.nix.binaryCaches;
                gc = {
                  automatic = true;
                  interval = { Hour = 24 * 7; };
                };
              };

              nixpkgs = {
                config = {
                  allowUnfree = true;
                  allowBroken = false;
                  allowUnsupportedSystem = false;
                };

                overlays = [
                  (se: su: { myInputs = inputs; })
                  emacs.overlay
                  nur.overlay
                  (final: prev: {
                    devenv = inputs.devenv.defaultPackage.${prev.system};
                    resource-id =
                      inputs.resource-id.defaultPackage.${prev.system};
                    ws-access-token =
                      inputs.ws-access-token.defaultPackage.${prev.system};
                  })

                  (import ./overlays)
                ];
              };
            }

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
            home-manager.nixosModules.home-manager
            {
              nix = {
                configureBuildUsers = true;
                settings = {
                  substituters = [
                    "https://utdemir.cachix.org"
                    "https://hs-nix-template.cachix.org"
                    "https://cache.nixos.org"
                    "https://nix-community.cachix.org"
                    "https://cachix.org/api/v1/cache/yuanwang-wf"
                    "https://cachix.org/api/v1/cache/emacs"
                    "https://cachix.org/api/v1/cache/devenv"
                  ];
                  trusted-public-keys = [
                    "utdemir.cachix.org-1:mDgucWXufo3UuSymLuQumqOq1bNeclnnIEkD4fFMhsw="
                    "hs-nix-template.cachix.org-1:/YbjZCrYAw7d9ayLayk7ZhBdTEkR10ZFmFuOq6ZJo4c="
                    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
                    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
                    "yuanwang-wf.cachix.org-1:P/RZ5Iuuuv2MYCNCnAsLfPGmgKMKeTwPaJclkrcwx80="
                    "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
                    "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
                  ];

                  auto-optimise-store = true;
                  max-jobs = 8;
                };
                # Avoid unwanted garbage collection when using nix-direnv
                extraOptions = ''
                  experimental-features = nix-command flakes
                  keep-outputs          = true
                  keep-derivations      = true
                '';
                # trustedBinaryCaches = config.nix.binaryCaches;
                gc = {
                  automatic = true;
                  interval = { Hour = 24 * 7; };
                };
              };

              nixpkgs = {
                config = {
                  allowUnfree = true;
                  allowBroken = false;
                  allowUnsupportedSystem = false;
                };

                overlays = [
                  (se: su: { myInputs = inputs; })
                  emacs.overlay
                  nur.overlay
                  (final: prev: {
                    devenv = inputs.devenv.defaultPackage.${prev.system};
                    resource-id =
                      inputs.resource-id.defaultPackage.${prev.system};
                    ws-access-token =
                      inputs.ws-access-token.defaultPackage.${prev.system};
                  })

                  (import ./overlays)
                ];
              };
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
            pkgs.ormolu
            # pkgs.haskellPackages.hnix
            pkgs.treefmt
            pkgs.nixfmt
          ];
        };
      });
}
