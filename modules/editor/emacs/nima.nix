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
  earlyDefaultEl ? null,
  earlyDefaultElFile ? null,
  monoFont ? "PragmataPro VF Mono Liga",
  font ? "PragmataPro Liga",
  defvar ? { },
  homeDirectory ? builtins.getEnv "HOME",
  workspaceDirectory ? "workspaces",
  lspStyle ? "eglot",
  featureOverrides ? { },
  extraModule ? { },
  rawOutput ? false,
}:

let
  inherit (pkgs.lib) optionalString;

  earlyDefvar = defvar;
  earlyDefaultElContent =
    if earlyDefaultEl == null then
      import ./early-init.nix {
        inherit monoFont font;
        isDarwin = pkgs.stdenv.isDarwin;
      }
    else
      earlyDefaultEl;
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
          ${earlyDefaultElContent}
        '';
      };

      _module.args = {
        inherit
          homeDirectory
          workspaceDirectory
          lspStyle
          ;
      };

      features = featureOverrides;
    }
    // extraModule;
}
