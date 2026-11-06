{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.rex;
in
with lib;
{
  options.modules.rex = {
    enable = mkEnableOption "Rex terminal multiplexer";
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username}.home.packages = [
      pkgs.rex
    ];
  };
}
