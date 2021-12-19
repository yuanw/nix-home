{ config, lib, pkgs, ... }:

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
  localConfig = {
    username = "yuanwang";
    name = "Yuan Wang";
    email = "yuan.wang@workiva.com";
    hostname = "wf17084";
    gpgKey = "19AD3F6B1A5BF3BF";
    homeDirectory = "/Users/yuanwang";
  };
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
