let
  sources = import ../nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in
{
  os = ./macintosh.nix;
  home-manager =
    if pkgs.stdenvNoCC.isDarwin then
      sources.home-manager + "/nix-darwin"
    else
      sources.home-manager + "/nix-darwin";
}
