# Staged nima entry point for the Emacs migration.
#
# This file intentionally returns an Emacs package, not a Home Manager
# `programs.emacs` module.  `nima` already wraps Emacs with packages, so callers
# should install the result directly via `home.packages` or `services.emacs.package`.
{
  pkgs,
  # Use emacs-overlay's development build by default, matching the current
  # repository preference for a newer Emacs than nixpkgs' stable `pkgs.emacs`.
  emacsPackage ? pkgs.emacs-git,
  earlyDefaultEl ? "",
  earlyDefaultElFile ? ../early-init.el,
  monoFont ? "PragmataPro VF Mono Liga",
  font ? "PragmataPro Liga",
  defvar ? { },
  featureOverrides ? { },
  extraModule ? { },
  rawOutput ? false,
}:

let
  inherit (pkgs.lib) optionalString;

  earlyDefvar = {
    my-mono-font = {
      value = monoFont;
      doc = "Monospace font family selected from Nix.";
    };
    my-font = {
      value = font;
      doc = "Proportional font family selected from Nix.";
    };
  }
  // defvar;
in
pkgs.mkNima {
  inherit rawOutput;

  featuresDir = ./features;

  module =
    { ... }:
    {
      package = emacsPackage;

      earlyDefaultEl = {
        defvar = earlyDefvar;
        elisp = ''
          ${optionalString (earlyDefaultElFile != null) (builtins.readFile earlyDefaultElFile)}
          ${earlyDefaultEl}
        '';
      };

      features = featureOverrides;
    }
    // extraModule;
}
