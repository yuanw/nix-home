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

  # The builder for one nix-darwin host.  Call it from inside THIS flake, from a
  # flake-parts module: its body closes over the inputs and over the primed input
  # set, and flake-parts binds those for a module of this flake.  It is not
  # callable across a repository boundary, and this was measured rather than
  # assumed: a consumer flake that calls it receives a primed input set in which
  # nix-casks.packages has none of the casks, so the WK host's own
  #     with inputs'.nix-casks.packages; [ betterdisplay ... ]
  # binds nothing at all and the names come out undefined.  See the STATUS block
  # of nix-home-private/flake.nix, which records that measurement and the two ways
  # out of it.  Do not copy this implementation into another repository: if it is
  # to be shared, it is shared by importing lib/mk-darwin-system.nix from a
  # module of this flake.
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

  # The package set for a system, from lib/mk-pkgs.nix -- the single definition.
  # perSystem in flake.nix uses that file directly; this export has no caller
  # today.  It is not a supported way for another repository to obtain packages:
  # the one measured attempt at that is recorded in nix-home-private/flake.nix.
  flake.mkPkgs =
    { system, extraOverlays ? [ ], fixesOverlay ? null }:
    import ../lib/mk-pkgs.nix {
      nixpkgs = inputs.nixpkgs;
      inherit system extraOverlays fixesOverlay;
    };
}
