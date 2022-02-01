
{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    xmonad.url = github:xmonad/xmonad;
    xmonad-contrib = github:xmonad/xmonad-contrib;
    taffybar = github:taffybar/taffybar;
  };
  outputs = {self, flake-utils, nixpkgs, xmonad, xmonad-contrib, taffybar}: {


  };
}
