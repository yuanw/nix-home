{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.antigravity-cli;
in
{
  options.modules.antigravity-cli = {
    enable = lib.mkEnableOption "antigravity-cli";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.llm-agents.antigravity-cli;
      description = "Which antigravity-cli package to use; provides the `agy` command.";
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ cfg.package ];

      # The prompt set is rendered into every enabled tool's skills dir by
      # agent-bundle (see modules/coding-agents/pi). antigravity-cli
      # contributes its own tools entry (skills -> ~/.gemini/config/skills,
      # MCP -> ~/.gemini/config/mcp_config.json).
      programs.agent-bundle = {
        tools = [ "antigravity" ];
      };
    };
  };
}
