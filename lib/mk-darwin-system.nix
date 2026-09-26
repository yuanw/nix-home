/* lib/mk-darwin-system.nix -- build one nix-darwin host system.

   This is the implementation.  hosts/default.nix calls it from inside its
   withSystem wrapper, and nix-home-private will call it through its nix-home
   input.  Do not copy this code into another repository: it is shared by
   importing this very file, the nix analogue of linking.  A copy is a fork,
   and the two halves will drift.

   The parameters below are named after the words used in the body, which was
   sliced out of hosts/default.nix by line number, not retyped.  Note that
   codingAgentsRoot is deliberately NOT a parameter: it names a directory in
   this repository, and seen from here it resolves to modules/coding-agents
   whichever side calls us, so a caller can neither forget it nor point it
   at a stale location.
*/
{
  inputs,
  inputs',
  config,
  hostname,
  system,
  loadPrivate,
  addtionsModule,
  pkgs ? null,
}: 
            inputs.nix-darwin.lib.darwinSystem (
              {
              specialArgs = {
                isDarwin = true;
                isNixOS = false;
                loadPrivate = loadPrivate;
                # nix-home-private/modules/work.nix builds its agent skills from this
                # shared public tree; it is given the root rather than copying it.
                codingAgentsRoot = ../modules/coding-agents;
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
                      (import ../modules/helpers/mergetools.nix)
                    ];

                    backupFileExtension = "hm-bak";
                    extraSpecialArgs = { inherit inputs; };
                  };
                }
                addtionsModule
              ];
            }

            # the packages, for whoever has them.  the framework's own channel for them
            # is args.pkgs: it installs them as _module.args.pkgs with mkforce, which
            # is how it refuses a duplicate definition.  the public call-site stands in
            # this flake's perSystem, where flake.nix has already provided them, so it
            # hands none and that provision stands.  a flake outside that scope --
            # nix-home-private -- hands them here, built from the exported
            # flake.pkgsOverlays.  handing them in both places would be a duplicate.
            //
            (if pkgs == null then { } else { inherit pkgs; })
            )
