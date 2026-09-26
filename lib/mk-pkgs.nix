/* lib/mk-pkgs.nix -- the package set for one system.

   There is one definition of it.  perSystem in flake.nix calls this to give the
   host modules their pkgs, and nix-home-private calls it through
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
