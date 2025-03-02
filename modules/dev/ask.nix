{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.dev.ask;
in
{

  options.modules.dev.ask = {
    enable = mkEnableOption "ask";
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {

      home.packages = [
        pkgs.haskellPackages.ask

      ];
    };
  };
}
