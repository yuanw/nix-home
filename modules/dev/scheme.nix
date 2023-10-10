{ config, lib, pkgs, ... }:
with lib;
let cfg = config.modules.dev.scheme;
in {

  options.modules.dev.scheme = { enable = mkEnableOption "scheme"; };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ pkgs.mitscheme ];
    };
  };
}
