let sources = import ../nix/sources.nix;
in
{
  macintosh = ./macintosh.nix;
  home-manager = sources.home-manager + "/nix-darwin";
}
