{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.modules.editors.emacs;
  emacsPatched = cfg.pkg.overrideAttrs (prev: {
    patches =
      (lib.optionals pkgs.stdenv.isDarwin [
        ./patches/system-appearance.patch
        ./patches/fix-ns-x-colors.patch
        ./patches/round-undecorated-frame.patch
      ])
      ++ prev.patches;
  });
in
import ./nima {
  inherit pkgs;
  emacsPackage = emacsPatched;
  monoFont = config.my.monoFont;
  font = config.my.font;
  homeDirectory = config.my.homeDirectory;
  workspaceDirectory = config.my.workspaceDirectory;
  lspStyle = cfg.lspStyle;
}
