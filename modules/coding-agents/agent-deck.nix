{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.agent-deck;
in
{
  options.modules.agent-deck = {
    enable = lib.mkEnableOption "agent-deck";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        pkgs.llm-agents.agent-deck
      ];
    };
  };
}
