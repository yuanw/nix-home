{ ... }:
{
  flake.myModules = {
    common.imports = [
      ./agenix.nix
      ./ai.nix
      ./code-agents/claude-code
      ./code-agents/cursor
      ./code-agents/droid.nix
      ./code-agents/forge.nix
      ./code-agents/pi
      ./speak2text.nix
      ./catppuccin.nix
      ./common.nix
      ./browsers/chromium.nix
      ./browsers/firefox
      ./browsers/librewolf.nix
      ./browsers/tor.nix
      ./dev/agda.nix
      ./dev/ask.nix
      ./dev/dart.nix
      ./dev/haskell.nix
      ./dev/haxe.nix
      ./dev/idris2.nix
      ./dev/go.nix
      ./dev/podman.nix

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
      ./nix-casks.nix
    ];
  };
}
