{
  description = "Yuan Nix-darwin/NixOS Home";

  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.11";
    nixpkgs.url = "nixpkgs/nixos-unstable";
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devenv.url = "github:cachix/devenv/latest";
    nix-colors.url = "github:misterio77/nix-colors";
    hosts.url = "github:StevenBlack/hosts";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
    emacs.url = "github:nix-community/emacs-overlay";
    reiryoku.url = "github:yuanw/reiryoku";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.darwin.follows = "darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ self
    , nixpkgs-stable
    , nixpkgs
    , darwin
    , home-manager
    , nur
    , emacs
    , flake-utils
    , hosts
    , reiryoku
    , agenix
    , nix-colors
    , ...
    }:
    let
      inherit (flake-utils.lib) eachDefaultSystem eachSystem;
      overlays = [
        emacs.overlay
        nur.overlay
        agenix.overlays.default
        (final: prev: {
          stable = nixpkgs-stable.legacyPackages.${prev.system};
          # use this variant if unfree packages are needed:
          # unstable = import nixpkgs-unstable {
          #   inherit system;
          #   config.allowUnfree = true;
          # };

        })
        (final: prev: {
          reiryoku-firmware = inputs.reiryoku.packages.${prev.system}.firmware;
          devenv = inputs.devenv.packages.${prev.system}.devenv;
        })
        (import ./hs-land/overlay.nix)
        (import ./overlays)
      ];

      # https://github.com/shaunsingh/nix-darwin-dotfiles/blob/main/flake.nix
      mkSystemConfig =
        { system
        , modules
        , isDarwin ? nixpkgs.lib.hasSuffix "-darwin" system
        , isNixOS ? !isDarwin
        , ...
        }:
        (if isDarwin then
          darwin.lib.darwinSystem
        else
          nixpkgs.lib.nixosSystem) {
          inherit system;
          specialArgs = { inherit nix-colors isNixOS isDarwin; };
          modules = modules ++ [{ nixpkgs.overlays = overlays; } ./modules]
            ++ (if isDarwin then
            ([
              agenix.darwinModules.age
              home-manager.darwinModules.home-manager
              ./macintosh.nix
            ]) else
            ([
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
              agenix.nixosModules.age
              home-manager.nixosModules.home-manager

            ]));

        };

      # idea borrowed from https://github.com/hardselius/dotfiles
      # mkDarwinSystem = { modules }:
      #   darwin.lib.darwinSystem {
      #     specialArgs = {
      #       inherit nix-colors;
      #       isNixOS = false;
      #       isDarwin = true;
      #     };

      #     system = "x86_64-darwin";
      #     modules = [
      #       { nixpkgs.overlays = overlays; }
      #       ./modules
      #       agenix.darwinModules.age
      #       home-manager.darwinModules.home-manager
      #       nix-colors.homeManagerModule
      #       ./macintosh.nix
      #     ] ++ modules;
      #   };
      # mkNixSystem = { modules }:
      #   nixpkgs.lib.nixosSystem {
      #     system = "x86_64-linux";
      #     specialArgs = inputs // {
      #       isNixOS = true;
      #       isDarwin = false;
      #     };
      #     modules = modules ++ [
      #       ./nixos_system.nix
      #       hosts.nixosModule
      #       {
      #         networking.stevenBlackHosts = {
      #           enable = true;
      #           blockFakenews = true;
      #           blockGambling = true;
      #           blockPorn = true;
      #           blockSocial = false;
      #         };
      #       }

      #       agenix.nixosModules.age
      #       home-manager.nixosModules.home-manager
      #       { nixpkgs.overlays = overlays; }

      #       ./modules
      #     ];
      #   };
    in
    {
      nixosConfigurations.adguard = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./modules/adguardhome-with-user.nix ./modules/adguard.nix ];
      };

      nixosConfigurations.asche = mkSystemConfig {
        system = "x86_64-linux";
        modules = [ ./machines/asche/configuration.nix ./hosts/asche.nix ];
      };

      darwinConfigurations = {
        yuanw = mkSystemConfig {
          system = "x86_64-darwin";
          modules = [ ./hosts/yuan-mac.nix ];
        };
        wf17084 = mkSystemConfig {
          system = "x86_64-darwin";
          modules = [ ./hosts/wf17084.nix ];
        };
      };

      asche = self.nixosConfigurations.asche.system;
      yuanw = self.darwinConfigurations.yuanw.system;
      wf17084 = self.darwinConfigurations.wf17084.system;

    } // eachDefaultSystem (system:
    let pkgs = import nixpkgs { inherit system; };
    in {
      devShells.adguard =
        pkgs.mkShell { buildInputs = with pkgs; [ awscli terraform nixfmt treefmt ]; };
    });
}
