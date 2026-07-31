{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      checks.mergetools = pkgs.callPackage ./mergetools.nix { };
      checks.nima-feature-helper = pkgs.callPackage ./nima-feature-helper.nix { };
    };
}
