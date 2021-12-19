{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.dev.julia;
  localConfig.username = "yuanwang";
in {
  options.modules.dev.julia = { enable = mkEnableOption "Julia"; };

  config = mkIf cfg.enable {
    home-manager.users.${localConfig.username} = {
      home.packages = [ pkgs.juliaMac ];
    };
  };
}
