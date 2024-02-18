{ config, lib, ... }:

with lib;
let
  cfg = config.modules.zellij;
in
{
  options.modules.zellij = {
    enable = mkEnableOption "zellij";
  };
  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      programs = {
        zellij = { enable = true; };

      };
    };
  };
}
