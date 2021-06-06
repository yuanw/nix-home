{ config, lib, pkgs, localConfig, ... }:

with lib;
let cfg = config.programs.julia;
in {
  options.modules.dev.julia = { enable = mkEnableOption "Julia"; };

  config = mkIf cfg.enable {
    home-manager.users.${localConfig.username} = {
      home.packages = [ pkgs.juliaMac ];
    };
  };
}
