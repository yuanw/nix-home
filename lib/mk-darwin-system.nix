/*
  lib/mk-darwin-system.nix -- build one nix-darwin host system.

  This is the implementation.  It is called from one place: the withSystem
  wrapper in hosts/default.nix, which is a flake-parts module of THIS flake.
  That is not a matter of taste.  The body closes over the inputs and over the
  primed input set, and flake-parts binds those for a module here; measured
  from the other side, a caller in another repository receives a primed set in
  which nix-casks.packages has none of the casks, so the host configuration
  that does  with inputs'.nix-casks.packages  binds nothing at all.  The WK
  host is therefore declared here, in hosts/wk01174.nix, while the Workiva
  modules it imports are contributed by nix-home-private through the flake-less
  nix-home-private input.  Do not copy this code into another repository: to
  duplicate an implementation is to fork it, and nothing notices the drift.

  The parameters below are named after the words used in the body, which was
  sliced out of hosts/default.nix by line number, not retyped.
*/
{
  inputs,
  inputs',
  config,
  hostname,
  system,
  addtionsModule,
}:
inputs.nix-darwin.lib.darwinSystem {
  specialArgs = {
    isDarwin = true;
    isNixOS = false;
    nurNoPkg = import inputs.nur {
      nurpkgs = import inputs.nixpkgs { system = system; };
    };
    packages = config.packages;
    inherit hostname inputs inputs';
  };
  modules = [
    {
      nixpkgs.hostPlatform = system;
    }
    inputs.nix-darwin-login-items.darwinModules.default
    inputs.home-manager.darwinModules.home-manager
    {
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
          inputs.nixpi.homeModules.default
          (import ../modules/helpers/mergetools.nix)
        ];

        backupFileExtension = "hm-bak";
        extraSpecialArgs = { inherit inputs; };
      };
    }
    addtionsModule
  ];
}
