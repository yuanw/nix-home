{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      checks.mergetools = pkgs.callPackage ./mergetools.nix { };
    };
}
