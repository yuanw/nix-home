/* lib/mk-pkgs.nix -- the package set for one system.

   There is one definition of it.  perSystem in flake.nix calls this to give the
   There is one definition of it, and one caller: perSystem in flake.nix, which
   installs it as the host modules' pkgs.  It is deliberately not exported to
   other repositories.  What was tried there is measured, not supposed: a caller
   outside a flake-parts module of this flake receives a primed input set in
   which nix-casks.packages has none of the casks, so the host configuration that
   does `with inputs'.nix-casks.packages' binds nothing.  To hand the packages
   over as well is worse than useless: the framework installs a supplied pkgs
   into _module.args.pkgs with mkforce, which then refuses the framework's own
   filling of that field and starves modules/browsers/librewolf-home.nix of
   pkgs.nur.  So: no second caller, and no export.  To duplicate this
   definition in another repository would be to fork it; import it, or do not
   build against it at all.
   inputs.nix-home.flake.mkPkgs so that the private host is built against the
   very same packages, at the very same nixpkgs revision.  Two definitions would
   mean two revisions, and nothing would notice the drift.

   The instantiation below was sliced out of flake.nix by line number, not
   retyped; three names were substituted, each asserted to have happened:
   inputs.nixpkgs, the fixes overlay, and the path to the packages directory
   (which from lib/ is one level up, and lands in the same place).
*/
{
  nixpkgs,
  system,
  fixesOverlay ? null,
  extraOverlays ? [ ],
}:
import nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
            };
            overlays =
              (nixpkgs.lib.optionals (builtins.elem system [
                "aarch64-linux"
                "x86_64-linux"
              ]) [ fixesOverlay ])
              ++ [ (import ../packages) ] ++ extraOverlays;
}
