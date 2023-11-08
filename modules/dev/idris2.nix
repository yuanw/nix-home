{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.idris2;
in {
  options.modules.dev.idris2 = { enable = mkEnableOption "idris2"; };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        pkgs.idris2
      ];
    };
  };
}
