{
  description = "Yuan Nix-darwin/NixOS Home";

  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixvim.url = "github:pta2002/nixvim";
    zig.url = "github:mitchellh/zig-overlay";
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
    astro-nvim = {
      url = "github:AstroNvim/AstroNvim";
      flake = false;
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
    , nixvim
    , astro-nvim
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
          mesa = nixpkgs-stable.legacyPackages.${prev.system}.mesa;
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
          specialArgs = { inherit nix-colors isNixOS isDarwin nixvim astro-nvim; };
          modules = modules ++ [{ nixpkgs.overlays = overlays; } ./modules]
            ++ (if isDarwin then
            ([
              agenix.darwinModules.age
              home-manager.darwinModules.home-manager
              ./macintosh.nix
              nixvim.nixDarwinModules.nixvim
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
              nixvim.nixosModules.nixvim
            ]));

        };
    in
    {
      nixosConfigurations.aws = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          { nixpkgs.overlays = [ agenix.overlays.default ]; }
          agenix.nixosModules.age
          ./modules/aws.nix
          ./modules/agenix.nix
        ];
      };

      nixosConfigurations.adguard = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          { nixpkgs.overlays = [ agenix.overlays.default ]; }
          agenix.nixosModules.age
          ./modules/aws.nix
          ./modules/adguradhome-with-user.nix
          ./modules/adguard.nix
          ./modules/agenix.nix
        ];
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
      adguard = self.nixosConfigurations.adguard.system;
      aws = self.nixosConfigurations.aws.system;

    } // eachDefaultSystem (system:
    let pkgs = import nixpkgs { inherit system; };
    in {
      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          awscli
          # mkpasswd
          # apacheHttpd
          lego
          terraform
          nixfmt
          treefmt
        ];
      };
    });
}
