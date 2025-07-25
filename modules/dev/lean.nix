{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.dev.lean;
in
{
  options.modules.dev.lean = {
    enable = mkEnableOption "lean";
    pkg = mkOption {
      type = types.package;
      default = pkgs.lean4;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        cfg.pkg
      ];
    };
  };

}
