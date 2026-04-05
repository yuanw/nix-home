{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.forge;
in
{
  options.modules.forge = {
    enable = lib.mkEnableOption "forge";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        pkgs.llm-agents.forge
      ];
    };
  };
}
