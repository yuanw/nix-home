{
  description = "Yuan Nix-darwin/NixOS Home";

  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.11";
    nixpkgs.url = "nixpkgs/nixos-unstable";
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hosts.url = "github:StevenBlack/hosts";
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
    emacs.url = "github:nix-community/emacs-overlay";
    qmk_firmware = {
      url = "github:yuanw/bastardkb-qmk/yuanw-master";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs-stable, nixpkgs, darwin, home-manager, nur
    , emacs, devshell, flake-utils, hosts, qmk_firmware, ... }:
    let
      inherit (flake-utils.lib) eachDefaultSystem eachSystem;
      overlays = [
        emacs.overlay
        nur.overlay
        (final: prev: {
          stable = nixpkgs-stable.legacyPackages.${prev.system};
          # use this variant if unfree packages are needed:
          # unstable = import nixpkgs-unstable {
          #   inherit system;
          #   config.allowUnfree = true;
          # };

        })
        (import ./hs-land/overlay.nix)
        (import ./overlays)
      ];

      # idea borrowed from https://github.com/hardselius/dotfiles
      mkDarwinSystem = { modules }:
        darwin.lib.darwinSystem {
          inputs = inputs;
          system = "x86_64-darwin";
          modules = [
            { nixpkgs.overlays = overlays; }

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
          specialArgs = inputs;
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
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ devshell.overlay ];
        };
      in {
        packages.firmware = pkgs.stdenv.mkDerivation rec {
          name = "firmware.hex";
          src = pkgs.fetchFromGitHub {
                owner = "yuanw";
      repo = "bastardkb-qmk";
      ref = "yuanw-master";
      sha256 = "YEKqqCLJQvFD3OaJvgD+OEDxXgPcjnaNpNtSPaDUS+M=";
     submodules = true;
          };

          buildInputs = with pkgs; [ qmk git ];

          # postUnpack = ''
          #   ln -s ${./.} $sourceRoot/keyboards/keebio/nyquist/keymaps/alternate
          # '';

          buildPhase = ''
            qmk compile -c -kb bastardkb/charybdis/3x5/v2/splinky_3  -km via
          '';

          installPhase = ''
            cp bastardkb_charybdis_3x5_v2_splinky_v3_*.bin $out
            cp bastardkb_charybdis_3x5_v2_splinky_v3_*.uf2 $out
          '';
        };
        devShell = pkgs.devshell.mkShell {
          name = "nix-home";
          imports = [ (pkgs.devshell.extraModulesDir + "/git/hooks.nix") ];
          git.hooks.enable = true;
          git.hooks.pre-commit.text = "${pkgs.treefmt}/bin/treefmt";
          packages = [ pkgs.treefmt pkgs.nixfmt ];
        };
      });
}
