# https://git.sr.ht/~rycee/configurations/tree/master/item/user/catppuccin.nix
{ pkgs, ... }:

{

  catppuccin.flavour = "mocha";
  programs.bat.catppuccin.enable = true;
  programs.starship.catppuccin.enable = true;
  programs.alacritty.catppuccin.enable = true;
}
