# https://git.sr.ht/~rycee/configurations/tree/master/item/user/catppuccin.nix
{ lib, pkgs, ... }:

let

  inherit (builtins) fromJSON readFile;

  catppuccinJson = fromJSON (readFile (pkgs.fetchurl {
    url =
      "https://raw.githubusercontent.com/catppuccin/palette/af11bc6d132d2e85cc192a9237f86fa9746c92c0/palette.json";
    sha256 = "sha256-AojVV7p4nm+1tSK9KN02YLwm14fkRr2pDRPUWYYkPeA=";
  }));

  loadTheme = name: catppuccinJson.${name};

in
{
  lib.catppuccin = loadTheme "mocha";

  catppuccin.flavour = "mocha";
  programs.bat.catppuccin.enable = true;
  programs.starship.catppuccin.enable = true;
  programs.alacritty.catppuccin.enable = true;
}
