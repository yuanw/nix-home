{ inputs, ... }:
let
  inherit (inputs) colmena;
in
{
  flake.colmenaHive = colmena.lib.makeHive {

    meta = {
      nixpkgs = import inputs.nixpkgs {
        localSystem.system = "x86_64-linux";
        config.allowUnfree = true;
        overlays = [ ];
      };
      nodeNixpkgs = {
        asche = import inputs.nixpkgs {
          localSystem.system = "x86_64-linux";
          config.allowUnfree = true;
          overlays = [ (import ../packages) ];
        };
        dgx-spark = import inputs.nixpkgs {
          localSystem.system = "aarch64-linux";
          config.allowUnfree = true;
          overlays = [ (import ../packages) ];
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
        # modules/browsers/librewolf.nix takes `hostname` as a module argument;
        # hosts/default.nix:31 passes the same literal on the other path.  The
        # hive specialArgs above is shared by every node, so it has to be here.
        _module.args.hostname = "asche";
        # health.nix and ai.nix name `inputs'` in their module arguments and the
        # `stable` overlay in modules/common.nix:57 reads a value out of it, but
        # this is not a perSystem module, so flake-parts does not provide it.
        # Only what asche actually reaches is built here, for asche's system.
        _module.args.inputs' = {
          nixpkgs-stable = {
            legacyPackages = import inputs.nixpkgs-stable {
              system = "x86_64-linux";
              config.allowUnfree = true;
            };
          };
        };
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          # Mirror of hosts/default.nix:49, which this list had drifted from: it
          # was missing the mergetools helper (asche sets home-manager mergetools
          # through my.browsers.librewolf.enable) and pi-flake.
          sharedModules = [
            inputs.betterfox.homeModules.betterfox
            inputs.catppuccin.homeModules.catppuccin
            inputs.direnv-instant.homeModules.direnv-instant
            inputs.mics-skills.homeModules.default
            inputs.flake-prompt.homeManagerModules.default
            inputs.mcp-servers-nix.homeManagerModules.default
            inputs.pi-flake.homeModules.default
            (import ../modules/helpers/mergetools.nix)
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

    silver =
      { ... }:
      {
        imports = [
          ./silver
        ];
        nixpkgs.hostPlatform = "x86_64-linux";
      };

    dgx-spark =
      { ... }:
      {
        imports = [
          ../modules/services/monitoring/pcp.nix
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
            inputs.flake-prompt.homeManagerModules.default
            inputs.mcp-servers-nix.homeManagerModules.default
          ];
          backupFileExtension = "hm-bak";
          extraSpecialArgs = { inherit inputs; };
        };
      };
  };
}
