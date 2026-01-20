{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.hledger;
in
{

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      programs.ledger = {
        enable = cfg.enable;
        package = pkgs.hledger;
      };
    };
  };
}
