{ config, lib, inputs', ... }:

with lib;
let cfg = config.modules.dev.roc;
in {

  options.modules.dev.roc = { enable = mkEnableOption "roc"; };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        inputs'.roc.cli
        inputs'.roc.lang-server
      ];
    };
  };
}
