{ inputs, ... }:
{
  # Paths to this repo's package overlay, exported as a flake output so that
  # nix-home-private can instantiate the very same pkgs instead of duplicating
  # the definition. Consumers do:
  #
  #     overlays = map (p: import p) inputs.nix-home.pkgsOverlays;
  #
  # Keep it a list of directories, each with a default.nix.
  flake.pkgsOverlays = [ ../packages ];

  # The builder for one nix-darwin host, exported so that nix-home-private can
  # build a host without re-implementing any of this configuration.  The private
  # side calls it as:
  #
  #     system = inputs.nix-home.flake.mkDarwinSystem {
  #       hostname = "WK01174";
  #       system   = "aarch64-darwin";
  #       addtionsModule = [ ./modules/work.nix ./modules/workMtab.nix ];
  #     };
  #
  # The implementation is lib/mk-darwin-system.nix, in this repository.  Do not
  # copy it into the private one: import it through the nix-home input, as above.
  flake.mkDarwinSystem =
    { hostname
    , system
    , config ? { packages = [ ]; }
    , loadPrivate ? false
    , addtionsModule ? { }
    , inputs' ? inputs
    }:
    import ../lib/mk-darwin-system.nix {
      inherit hostname system config loadPrivate addtionsModule inputs inputs';
    };

  flake.myModules = {
    common.imports = [
      ./agenix.nix
      ./ai.nix
      ./browsers/default.nix
      ./browsers/chromium.nix
      ./browsers/librewolf.nix
      ./browsers/tor.nix
      ./catppuccin.nix
      ./coding-agents/cursor
      ./coding-agents/droid.nix
      ./coding-agents/forge.nix
      ./coding-agents/herdr
      ./coding-agents/hermes-agent.nix
      ./coding-agents/hunk.nix
      ./coding-agents/open-code-review.nix
      ./coding-agents/pi
      ./common.nix
      ./nix-custom-conf.nix
      ./dev/agda.nix
      ./dev/ask.nix
      ./dev/dart.nix
      ./dev/gcloud.nix
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
      ./speech2text/speak2text.nix
      ./terminal
      ./terminal-multiplexer/tmux.nix
      ./terminal-multiplexer/zellij.nix
      ./typing
    ];

    linux.imports = [
      ./cockpit.nix
      ./ds4.nix
      ./lance.nix
      ./mullvad.nix
      ./qmk.nix
      ./nixos_system.nix
      ./services/monitoring/pcp.nix
      ./wm/xmonad.nix
    ];
    darwin.imports = [
      ./browsers/librewolf-darwin.nix
      ./browsers/browser-cli-darwin.nix
      ./coding-agents/hermes-agent-darwin.nix
      ./brew.nix
      ./health.nix
      ./editor/emacs/emacs-macos.nix
      ./wm/yabai.nix
      ./login-items.nix
      ./macintosh.nix
      ./mouseless
      ./neru
      ./nix-casks.nix
    ];
  };
}
