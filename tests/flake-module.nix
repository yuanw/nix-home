{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      checks.mergetools = pkgs.callPackage ./mergetools.nix { };
      checks.github-widget = pkgs.callPackage ./github-widget.nix { };
      checks.transcribe = pkgs.callPackage ./transcribe.nix { };
    };
}
