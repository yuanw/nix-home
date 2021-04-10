# why not use stdenv isDarwin function
# https://github.com/nix-community/home-manager/issues/414
{ lib, isDarwin ? false, isNixOS ? false }:

with lib;
let
  loadModule = file: { condition ? true }: { inherit file condition; };
  allModules = [
    (loadModule ./dart.nix { })
    (loadModule ./editor.nix { })
    (loadModule ./haskell.nix { })
    (loadModule ./hledger.nix { })
    (loadModule ./hosts.nix { })
    (loadModule ./node.nix { })
    (loadModule ./python.nix { })
    (loadModule ./terminal.nix { })
    (loadModule ./wm/yabai.nix { condition = isDarwin; })
    (loadModule ./workShell.nix { })
  ];
  modules = map (getAttr "file") (filter (getAttr "condition") allModules);
in modules
