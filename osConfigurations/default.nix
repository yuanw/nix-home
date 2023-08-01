{ self, inputs, system, ... }:
{
  flake = {
    # All nixos/nix-darwin configurations are kept here.
    # nixosModules = {
    #   # Common nixos/nix-darwin configuration shared between Linux and macOS.
    #   # common.imports = [
    #   #   # ../modules/common.nix
    #   # ];
    #       # NixOS specific configuration
    #   linux = { ... }: { };
    #   # nix-darwin specific configuration
    #   darwin.imports = [
    #     ../macintosh.nix
    #   ];
    # };
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
        modules = [ ./machines/asche/configuration.nix ./hosts/asche.nix ];
      };
    };
    darwinConfigurations = {
      yuanw = self.nixos-flake.lib.mkMacosSystem {
        system = "x86_64-darwin";
        modules = [ ../hosts/yuan-mac.nix ];
      };
      WK01174 = self.nixos-flake.lib.mkMacosSystem "aarch64-darwin" {
        imports = [
          inputs.self.nixosModules.common
          inputs.self.nixosModules.darwin
          ../hosts/wk01174.nix
        ];
      };
      wf17084 = self.nixos-flake.lib.mkMacosSystem "aarch64-darwin" {
        imports = [
          inputs.self.nixosModules.common
          inputs.self.nixosModules.darwin
          ../hosts/wf17084.nix
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
