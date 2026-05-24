{ inputs, ... }:
let
  inherit (inputs) colmena;
in
{
  flake.colmenaHive = colmena.lib.makeHive {

    meta = {
      nixpkgs = import inputs.nixpkgs {
        system = "x86_64-linux";
        config.allowUnfree = true;
        overlays = [ ];
      };
      nodeNixpkgs = {
        dgx-spark = import inputs.nixpkgs {
          system = "aarch64-linux";
          config.allowUnfree = true;
          overlays = [ ];
        };
      };
      specialArgs = {
        inherit inputs;
        isDarwin = false;
        isNixOS = true;
        nurNoPkg = null;
      };
    };

    defaults =
      { ... }:
      {
        nixpkgs.config.allowUnfree = true;
      };

    asche =
      { ... }:
      {
        imports = [
          inputs.home-manager.nixosModules.home-manager
          ./asche
        ];
        nixpkgs.hostPlatform = "x86_64-linux";
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          sharedModules = [
            inputs.betterfox.homeModules.betterfox
            inputs.catppuccin.homeModules.catppuccin
            inputs.direnv-instant.homeModules.direnv-instant
            inputs.mics-skills.homeModules.default
            inputs.git-ai.homeManagerModules.default
            inputs.mcp-servers-nix.homeManagerModules.default
          ];
          backupFileExtension = "hm-bak";
          extraSpecialArgs = { inherit inputs; };
        };
      };

    misfit =
      { ... }:
      {
        imports = [
          ./misfit
        ];
        nixpkgs.hostPlatform = "x86_64-linux";
      };

    dgx-spark =
      { ... }:
      {
        imports = [
          inputs.home-manager.nixosModules.home-manager
          ./dgx-spark
        ];
        nixpkgs.hostPlatform = "aarch64-linux";
        nixpkgs.config.cudaSupport = true;
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          sharedModules = [
            inputs.betterfox.homeModules.betterfox
            inputs.catppuccin.homeModules.catppuccin
            inputs.direnv-instant.homeModules.direnv-instant
            inputs.mics-skills.homeModules.default
            inputs.git-ai.homeManagerModules.default
            inputs.mcp-servers-nix.homeManagerModules.default
          ];
          backupFileExtension = "hm-bak";
          extraSpecialArgs = { inherit inputs; };
        };
      };
  };
}
