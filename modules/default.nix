{ pkgs, lib, ... }:

{
  imports = [
    ./dart.nix
    ./editor.nix
    ./haskell.nix
    ./hledger.nix
    ./hosts.nix
    ./node.nix
    ./python.nix
    ./workShell.nix
  ] ++ lib.optionals pkgs.stdenvNoCC.isDarwin [ ./wm/yabai.nix ];
}
