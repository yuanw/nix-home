{ self, inputs, system, ... } :
let
  nixosSystem = args:
    inputs.nixpkgs.lib.nixosSystem ({ specialArgs = { inherit inputs; }; } // args);
  darwinSystem = args:
    inputs.nix-darwin.lib.darwinSystem ({ specialArgs = { inherit inputs; }; } // args);
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

      asche = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./machines/asche/configuration.nix ./asche.nix ];
      };
    };
    darwinConfigurations = {
      yuanw = self.nixos-flake.lib.mkMacosSystem "x86_64-darwin" {
        imports = [
          inputs.self.nixosModules.common
          inputs.self.nixosModules.darwin
          ./yuan-mac.nix
        ];
      };
      WK01174 =  darwinSystem  {
        system =  "aarch64-darwin";
        modules =  [
          inputs.self.nixosModules.common
          inputs.self.nixosModules.darwin
          ./wk01174.nix
        ];
      };
      wf17084 = self.nixos-flake.lib.mkMacosSystem "x86_64-darwin" {
        imports = [
          inputs.self.nixosModules.common
          inputs.self.nixosModules.darwin
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
