{ config, lib, pkgs, localConfig, ... }:

with lib;
let cfg = config.programs.node;
in {
  options.programs.node = { enable = mkEnableOption "node"; };

  config = mkIf cfg.enable {
    home-manager.users.${localConfig.username} = {
      home.packages = [ pkgs.nodejs pkgs.yarn ];
    };
  };
}
