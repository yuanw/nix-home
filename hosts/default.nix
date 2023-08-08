{ self, inputs, system, ... }:
let
  nixosSystem = args:
    inputs.nixpkgs.lib.nixosSystem ({ specialArgs = { inherit inputs; isDarwin = false; isNixOS = true; }; } // args);
  darwinSystem = args:
    inputs.nix-darwin.lib.darwinSystem ({ specialArgs = { inherit inputs; isDarwin = true; isNixOS = false; }; } // args);
in
{
  flake = {
    nixosConfigurations = {
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

      asche = nixosSystem {
        system = "x86_64-linux";
        modules = [
          inputs.self.nixosModules.common
          inputs.self.nixosModules.linux
          ../machines/asche/configuration.nix
          ./asche.nix
        ];
      };
    };
    darwinConfigurations = {
      yuanw = darwinSystem {
        system = "x86_64-darwin";
        modules = [
          inputs.self.nixosModules.common
          inputs.self.nixosModules.darwin
          ./yuan-mac.nix
        ];
      };
      WK01174 = darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./wk01174.nix
        ];
      };
      wf17084 = darwinSystem {
        system = "x86_64-darwin";
        modules = [
          ./wf17084.nix
        ];
      };


    };

  };
  perSystem = { system, ... }: {
    packages.asche = self.nixosConfigurations.asche.config.system.build.toplevel;
    packages.yuanw = self.darwinConfigurations.yuanw.system;
    packages.wf17084 = self.darwinConfigurations.wf17084.system;
    packages.wk01174 = self.darwinConfigurations.WK01174.system;
    #     adguard = self.nixosConfigurations.adguard.system;
    #     aws = self.nixosConfigurations.aws.system;

  };

}
