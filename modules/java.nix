{ config, lib, pkgs, ... }:

with lib;
let cfg = config.programs.java;
in {
  options.programs.java = { enable = mkEnableOption "java"; };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ pkgs.gradle ];
    };
  };
}
