{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.dev.go;
in
{
  options.modules.dev.go = {
    enable = mkEnableOption "go";
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = with pkgs; [
        gotools

      ];
      programs = {
        go = {

          enable = true;
          goPath = "go";
          telemetry.mode = "off";

        };

      };
    };
  };
}
