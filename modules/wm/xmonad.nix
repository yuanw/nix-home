{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.modules.wm.xmonad;
    xmonad-env = pkgs.haskellPackages.ghcWithHoogle (hp:
    with hp; [
      xmobar
      xmonad
      xmonad-contrib
      xmonad-extras
    ]);

in
{
  options.modules.wm.xmonad = {enable = mkEnableOption "xmonad";};

  config = mkIf cfg.enable  {

    home-manager.users.${config.my.username} = {
      home.packages = [xmonad-env ];
        services.caffeine.enable = true;
        services.xscreensaver.enable = true;
    };
  };


}
