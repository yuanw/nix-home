{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.browsers.chromium;

in
{
  options.modules.browsers.chromium = {
    enable = mkEnableOption "chromium";
    pkg = mkOption {
      type = types.package;
      default = pkgs.brave;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      programs.chromium.enable = true;
      programs.chromium.package = cfg.pkg;
    };
  };

}
