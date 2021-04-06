{ lib, pkgs, ... }:

with lib;
let
  hostPlatform = pkgs.stdenv.hostPlatform;
  loadModule = file: { condition ? true }: { inherit file condition; };
  allModules = [
    (loadModule ./dart.nix { })
    (loadModule ./editor.nix { })
    (loadModule ./haskell.nix { })
    (loadModule ./hledger.nix { })
    (loadModule ./hosts.nix { })
    (loadModule ./node.nix { })
    (loadModule ./python.nix { })
    (loadModule ./wm/yabai.nix { condition = hostPlatform.isDarwin; })
    (loadModule ./workShell.nix { })
  ];
  modules = map (getAttr "file") (filter (getAttr "condition") allModules);
in modules
