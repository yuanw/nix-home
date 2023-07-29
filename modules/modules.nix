# why not use stdenv isDarwin function
# https://github.com/nix-community/home-manager/issues/414
{ lib, isDarwin, isNixOS, ... }:

with lib;
let
  loadModule = file: { condition ? true }: { inherit file condition; };
  allModules = [
    (loadModule ./browsers/firefox.nix { })
    (loadModule ./dev/agda.nix { })
    (loadModule ./dev/dart.nix { })
    (loadModule ./dev/haskell.nix { })
    (loadModule ./dev/julia.nix { })
    (loadModule ./dev/node.nix { })
    (loadModule ./dev/python.nix { })
    (loadModule ./dev/zig.nix { })
    (loadModule ./editor.nix { })
    (loadModule ./hosts.nix { })
    (loadModule ./helix.nix { })
    (loadModule ./neovim { })
    (loadModule ./hledger.nix { })
    (loadModule ./common.nix { })
    (loadModule ./agenix.nix { })
    (loadModule ./settings.nix { })
    (loadModule ./terminal { })
    (loadModule ./typing { })
    (loadModule ./health.nix { })
    # (loadModule ./wm/amethyst.nix { condition = isDarwin; })
    #
    (loadModule ./qmk.nix { condition = isNixOS; })
    (loadModule ./wm/xmonad.nix { condition = isNixOS; })
    (loadModule ./adguradhome-with-user.nix { condition = isNixOS; })
    (loadModule ./moonlander.nix { condition = isNixOS; })
    (loadModule ./wm/yabai.nix { condition = isDarwin; })
    # (loadModule ./wm/sketchybar.nix { condition = isDarwin; })
    (loadModule ./brew.nix { condition = isDarwin; })
    (loadModule ./workShell.nix { condition = isDarwin; })
  ];
  modules = map (getAttr "file") (filter (getAttr "condition") allModules);
in
modules
