{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.browsers.tor;

in
{
  options.modules.browsers.tor = {
    enable = mkEnableOption "tor browser";
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ pkgs.tor-browser ];
    };
  };

}
