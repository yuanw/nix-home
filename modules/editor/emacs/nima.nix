# Staged nima entry point for the Emacs migration.
#
# This file intentionally returns an Emacs package, not a Home Manager
# `programs.emacs` module.  `nima` already wraps Emacs with packages, so callers
# should install the result directly via `home.packages` or `services.emacs.package`.
{
  pkgs,
  lib ? pkgs.lib,
  # Pass only the small module config slices this builder needs.
  myConfig ? null,
  emacsConfig ? null,
  # Standalone/testing overrides.
  emacsPackage ? null,
  earlyDefaultEl ? null,
  earlyDefaultElFile ? null,
  monoFont ? null,
  font ? null,
  defvar ? { },
  homeDirectory ? null,
  workspaceDirectory ? null,
  lspStyle ? null,
  featureOverrides ? { },
  extraModule ? { },
  rawOutput ? false,
}:

let
  inherit (lib) optionalString;

  emacsPackage' =
    if emacsPackage != null then
      emacsPackage
    else if emacsConfig != null then
      emacsConfig.pkg.overrideAttrs (prev: {
        patches =
          (lib.optionals pkgs.stdenv.isDarwin [
            ./patches/system-appearance.patch
            ./patches/fix-ns-x-colors.patch
            ./patches/round-undecorated-frame.patch
          ])
          ++ prev.patches;
      })
    else
      pkgs.emacs-git;
  monoFont' =
    if monoFont != null then
      monoFont
    else if myConfig != null then
      myConfig.monoFont
    else
      "PragmataPro VF Mono Liga";
  font' =
    if font != null then
      font
    else if myConfig != null then
      myConfig.font
    else
      "PragmataPro Liga";
  homeDirectory' =
    if homeDirectory != null then
      homeDirectory
    else if myConfig != null then
      myConfig.homeDirectory
    else
      builtins.getEnv "HOME";
  workspaceDirectory' =
    if workspaceDirectory != null then
      workspaceDirectory
    else if myConfig != null then
      myConfig.workspaceDirectory
    else
      "workspaces";
  lspStyle' =
    if lspStyle != null then
      lspStyle
    else if emacsConfig != null then
      emacsConfig.lspStyle
    else
      "eglot";

  earlyDefvar = defvar;
  earlyDefaultElContent =
    if earlyDefaultEl == null then
      import ./early-init.nix {
        monoFont = monoFont';
        font = font';
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
      package = emacsPackage';

      earlyDefaultEl = {
        defvar = earlyDefvar;
        elisp = ''
          ${optionalString (earlyDefaultElFile != null) (builtins.readFile earlyDefaultElFile)}
          ${earlyDefaultElContent}
        '';
      };

      _module.args = {
        homeDirectory = homeDirectory';
        workspaceDirectory = workspaceDirectory';
        lspStyle = lspStyle';
      };

      features = featureOverrides;
    }
    // extraModule;
}
