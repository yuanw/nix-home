{ ... }:
{
  flake.myModules = {
    common.imports = [
      ./agenix.nix
      ./ai.nix
      ./browsers/chromium.nix
      ./browsers/firefox
      ./browsers/librewolf.nix
      ./browsers/tor.nix
      ./catppuccin.nix
      ./coding-agents/claude-code
      ./coding-agents/cursor
      ./coding-agents/droid.nix
      ./coding-agents/forge.nix
      ./coding-agents/pi
      ./common.nix
      ./dev/agda.nix
      ./dev/ask.nix
      ./dev/dart.nix
      ./dev/go.nix
      ./dev/haskell.nix
      ./dev/haxe.nix
      ./dev/idris2.nix
      ./dev/java.nix
      ./dev/julia.nix
      ./dev/kotlin.nix
      ./dev/lean.nix
      ./dev/node.nix
      ./dev/playwright.nix
      ./dev/podman.nix
      ./dev/python.nix
      ./dev/racket.nix
      ./dev/scheme.nix
      ./dev/zig.nix
      ./editor/emacs
      ./helix.nix
      ./settings.nix
      ./speak2text/speak2text.nix
      ./terminal
      ./terminal-multiplexer/tmux.nix
      ./terminal-multiplexer/zellij.nix
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
      ./nix-casks.nix
    ];
  };
}
