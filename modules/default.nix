{ ... }:
{
  flake.nixosModules = {
    common.imports = [
      ./agenix.nix
      ./ai.nix
      ./common.nix
      ./browsers/firefox.nix
      ./dev/agda.nix
      ./dev/dart.nix
      ./dev/haskell.nix
      ./dev/haxe.nix
      ./dev/java.nix
      ./dev/julia.nix
      ./dev/node.nix
      ./dev/python.nix
      ./dev/zig.nix
      ./dev/scheme.nix
      ./helix.nix
      ./editor/doom
      ./editor/neovim
      ./settings.nix
      ./terminal
      ./typing
      ./workShell.nix
    ];
    linux.imports = [
      ./qmk.nix
      ./nixos_system.nix
      ./wm/xmonad.nix
    ];
    darwin.imports = [
      ./brew.nix
      ./health.nix
      ./wm/yabai.nix
      ./macintosh.nix
    ];
  };
}
