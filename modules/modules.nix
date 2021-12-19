# why not use stdenv isDarwin function
# https://github.com/nix-community/home-manager/issues/414

{ config, pkgs, ... }: {
  imports = [
    ./browsers/firefox.nix
    ./dev/dart.nix
    ./dev/haskell.nix
    ./dev/julia.nix
    ./dev/node.nix
    ./dev/python.nix
    ./editor.nix
    ./hosts.nix
    ./hledger.nix
    ./settings.nix
    ./terminal
    ./wm/yabai.nix
    ./brew.nix
    ./workShell.nix
  ];
}
