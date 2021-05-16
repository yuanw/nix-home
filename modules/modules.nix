# why not use stdenv isDarwin function
# https://github.com/nix-community/home-manager/issues/414
{ lib, isDarwin ? false, isNixOS ? false }:

with lib;
let
  loadModule = file: { condition ? true }: { inherit file condition; };
  allModules = [
    (loadModule ./browsers/firefox.nix { })
    (loadModule ./dev/dart.nix { })
    (loadModule ./dev/haskell.nix { })
    (loadModule ./dev/java.nix { })

    (loadModule ./dev/julia.nix { })
    (loadModule ./dev/node.nix { })
    (loadModule ./dev/python.nix { })
    (loadModule ./editor.nix { })
    (loadModule ./hosts.nix { })
    (loadModule ./hledger.nix { })
    (loadModule ./terminal { })
    (loadModule ./wm/yabai.nix { condition = isDarwin; })
    (loadModule ./workShell.nix { })
  ];
  modules = map (getAttr "file") (filter (getAttr "condition") allModules);
in modules
