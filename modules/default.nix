{ ... }:
{
  flake.myModules = {
    common.imports = [
      ./agenix.nix
      ./ai.nix
      ./catppuccin.nix
      ./common.nix
      ./browsers/chromium.nix
      ./browsers/firefox.nix
      ./browsers/librewolf.nix
      ./dev/agda.nix
      ./dev/ask.nix
      ./dev/dart.nix
      ./dev/haskell.nix
      ./dev/haxe.nix
      ./dev/idris2.nix
      ./dev/go.nix

      ./dev/lean.nix
      ./dev/java.nix
      ./dev/julia.nix
      ./dev/kotlin.nix
      ./dev/node.nix
      ./dev/playwright.nix
      ./dev/python.nix
      ./dev/racket.nix
      ./dev/zig.nix
      ./dev/scheme.nix
      ./helix.nix

      ./terminal-multiplexer/zellij.nix
      ./terminal-multiplexer/tmux.nix
      ./editor/emacs
      ./settings.nix
      ./terminal
      ./typing
    ];

    linux.imports = [
      ./mullvad.nix
      ./qmk.nix
      ./nixos_system.nix
      ./wm/xmonad.nix
    ];
    darwin.imports = [
      ./brew.nix
      ./health.nix
      ./editor/emacs/emacs-macos.nix
      ./wm/yabai.nix
      ./macintosh.nix
      ./mouseless

    ];
  };
}
