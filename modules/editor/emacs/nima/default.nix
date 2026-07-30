# Staged nima entry point for the Emacs migration.
#
# This file intentionally returns an Emacs package, not a Home Manager
# `programs.emacs` module.  `nima` already wraps Emacs with packages, so callers
# should install the result directly via `home.packages` or `services.emacs.package`.
{
  pkgs,
  emacsPackage ? pkgs.emacs,
  earlyDefaultEl ? "",
  featureOverrides ? { },
  extraModule ? { },
}:

pkgs.mkNima {
  featuresDir = ./features;

  module =
    { ... }:
    {
      package = emacsPackage;

      earlyDefaultEl.elisp = earlyDefaultEl;

      features = featureOverrides;
    }
    // extraModule;
}
