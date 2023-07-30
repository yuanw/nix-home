{ self, inputs, system, ... }:
let

  overlays = [
    inputs.emacs.overlay
    inputs.nur.overlay
    inputs.agenix.overlays.default
    (_final: prev: {
      stable = inputs.nixpkgs-stable.legacyPackages.${prev.system};
      mesa = inputs.nixpkgs-stable.legacyPackages.${prev.system}.mesa;
    })
    (_final: prev: {
      reiryoku-firmware = inputs.reiryoku.packages.${prev.system}.firmware;
      devenv = inputs.devenv.packages.${prev.system}.devenv;
    })
    (import ./hs-land/overlay.nix)
    (import ./overlays)
  ];

  mkSystemConfig =
    { system
    , modules
    , isDarwin ? inputs.nixpkgs.lib.hasSuffix "-darwin" system
    , isNixOS ? !isDarwin
    , ...
    }:
    (if isDarwin then
      inputs.darwin.lib.darwinSystem
    else
      inputs.nixpkgs.lib.nixosSystem) {
      inherit system;
      specialArgs = { inherit inputs isNixOS isDarwin; };
      modules = modules ++ [{ nixpkgs.overlays = overlays; } ../modules]
        ++ (if isDarwin then
        ([
          inputs.agenix.darwinModules.age
          inputs.home-manager.darwinModules.home-manager
          ../macintosh.nix
        ]) else
        ([
          ../nixos_system.nix
          inputs.hosts.nixosModule
          {
            networking.stevenBlackHosts = {
              enable = true;
              blockFakenews = true;
              blockGambling = true;
              blockPorn = true;
              blockSocial = false;
            };
          }
          inputs.agenix.nixosModules.age
          inputs.home-manager.nixosModules.home-manager
        ]));

    };
in
{
  flake.nixosConfigurations = {
    aws = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        { nixpkgs.overlays = [ inputs.agenix.overlays.default ]; }
        inputs.agenix.nixosModules.age
        ../modules/aws.nix
        ../modules/agenix.nix
      ];
    };

    adguard = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        { nixpkgs.overlays = [ inputs.agenix.overlays.default ]; }

        inputs.agenix.nixosModules.age
        ../modules/aws.nix
        ../modules/adguradhome-with-user.nix
        ../modules/adguard.nix
        ../modules/agenix.nix
      ];
    };

    asche = mkSystemConfig {
      system = "x86_64-linux";
      modules = [ ./machines/asche/configuration.nix ./hosts/asche.nix ];
    };
    flake.darwinConfigurations = {
      yuanw = mkSystemConfig {
        system = "x86_64-darwin";
        modules = [ ../hosts/yuan-mac.nix ];
      };
      WK01174 = mkSystemConfig {
        system = "aarch64-darwin";
        modules = [ ../hosts/wk01174.nix ];
      };
      wf17084 = mkSystemConfig {
        system = "x86_64-darwin";
        modules = [ ../hosts/wf17084.nix ];
      };
    };

  };
  perSystem = { system, ... }: {
    # asche = self.nixosConfigurations.asche.config.system.build.toplevel;
    #     yuanw = self.darwinConfigurations.yuanw.system;
    #     wf17084 = self.darwinConfigurations.wf17084.system;
    packages.wk01174 = self.darwinConfigurations.WK01174.system;
    #     adguard = self.nixosConfigurations.adguard.system;
    #     aws = self.nixosConfigurations.aws.system;

  };

}
