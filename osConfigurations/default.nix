{ self, inputs, system, ... }:
{
  flake = {
    # All nixos/nix-darwin configurations are kept here.
    # nixosModules = {
    #   # Common nixos/nix-darwin configuration shared between Linux and macOS.
    #   common = { pkgs, ... }: {
    #     environment.systemPackages = with pkgs; [
    #       hello
    #     ];
    #   };
    #   # NixOS specific configuration
    #   linux = { ... }: { };
    #   # nix-darwin specific configuration
    #   darwin.imports = [
    #     # ../macintosh.nix
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
      WK01174 = self.nixos-flake.lib.mkSystemConfig {
        system = "aarch64-darwin";
        modules = [
          ../hosts/wk01174.nix
          # self.nixosModules.darwin
        ];
      };
      # WK01174 =   inputs.darwin.lib.darwinSystem {
      #   system = "aarch64-darwin";
      #   modules = [
      #     inputs.agenix.darwinModules.age
      #     inputs.home-manager.darwinModules.home-manager
      #     ../macintosh.nix
      #     ../hosts/wk01174.nix
      #      # ../modules
      #   ];
      # };
      wf17084 = self.nixos-flake.lib.mkMacosSystem {
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
