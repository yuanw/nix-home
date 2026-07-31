{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      checks.mergetools = pkgs.callPackage ./mergetools.nix { };
      checks.nima-feature-helper = pkgs.callPackage ./nima-feature-helper.nix { };
      checks.nima-use-package-helper = pkgs.callPackage ./nima-use-package-helper.nix { };
    };
}
