{ config, lib, pkgs, localConfig, ... }:

with lib;
let cfg = config.programs.java;
in {
  options.programs.java = { enable = mkEnableOption "java"; };

  config = mkIf cfg.enable {
    home-manager.users.${localConfig.username} = {
      home.packages = [ pkgs.gradle pkgs.maven pkgs.jdk ];
    };
  };
}
