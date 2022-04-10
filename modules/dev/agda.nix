{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.agda;
in {
  options.modules.dev.agda = { enable = mkEnableOption "agda"; };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ (pkgs.agda.withPackages (p: [ p.standard-library ])) ];
      programs = {
        zsh = { sessionVariables = { AGDA_DIR = "$XDG_CONFIG_HOME/agda"; }; };
      };
      xdg.configFile = { "agda/defaults".text = "standard-library"; };
    };
  };
}
