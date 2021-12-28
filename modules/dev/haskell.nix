{ config, lib, pkgs, localConfig, ... }:

with lib;
let
  cfg = config.programs.haskell;
  haskell-env = pkgs.haskellPackages.ghcWithHoogle (hp:
    with hp; [
      # xmonad
      # xmonad-contrib
      # xmonad-extras
      apply-refact
      hlint
      haskell-language-server
      # xmobar
    ]);
in {
  options.programs.haskell = { enable = mkEnableOption "haskell"; };

  config = mkIf cfg.enable {
    home-manager.users.${localConfig.username} = {
      home.packages = [
        haskell-env
      ];
    };
  };
}
