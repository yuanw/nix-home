{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.droid;
in
{
  options.modules.droid = {
    enable = lib.mkEnableOption "droid";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        pkgs.llm-agents.droid
      ];
    };
  };
}
