{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      checks.mergetools = pkgs.callPackage ./mergetools.nix { };
      checks.transcribe = pkgs.callPackage ./transcribe.nix { };
    };
}
