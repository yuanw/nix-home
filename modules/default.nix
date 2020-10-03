{
  macintosh    = ./macintosh.nix;
  home-manager = "${builtins.fetchTarball https://github.com/nix-community/home-manager/archive/master.tar.gz}/nix-darwin";
}
