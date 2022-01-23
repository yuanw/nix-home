{ config, lib, pkgs,  ... }:

with lib;
let cfg = config.modules.dev.julia;
in {
  options.modules.dev.julia = { enable = mkEnableOption "Julia"; };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ pkgs.juliaMac ];
    };
  };
}
