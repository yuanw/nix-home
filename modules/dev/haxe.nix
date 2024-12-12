{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.dev.haxe;
in
{

  options.modules.dev.haxe = {
    enable = mkEnableOption "haxe";
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ pkgs.haxe ];
    };
  };
}
