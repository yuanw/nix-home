{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.agda;
in {
  options.modules.dev.agda= { enable = mkEnableOption "agda"; };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        (pkgs.agda.withPackages (p: [ p.standard-library ]))
                      ];
      # home.file = {
      #   ".agda/defaults" = "standard-libaray";
      # };
    };
  };
}
