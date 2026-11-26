{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.cursor;
in
{
  options.modules.cursor = {
    enable = lib.mkEnableOption "cursor";

    agentFiles = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      # Cursor's user-level subagents, linked straight from the source repo:
      # agent-bundle has no subagent type, so it cannot render this one. The
      # same pstack agent reaches Pi as a degraded skill (the prompt set in
      # modules/coding-agents/prompts).
      default = {
        comment-sicko = inputs.cursor-plugins + /pstack/agents/comment-sicko.md;
      };
      description = "Subagent markdown files, linked into ~/.cursor/agents/<name>.md.";
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = lib.mkMerge [
      { home.packages = [ (lib.lowPrio pkgs.llm-agents.cursor-agent) ]; }

      {
        # The prompt set is rendered into every enabled tool's skills dir
        # by agent-bundle (see modules/coding-agents/pi). cursor contributes
        # its own tools entry; .cursor/agents keeps the subagent link above.
        programs.agent-bundle = {
          tools = [ "cursor" ];
        };
      }

      (lib.mkIf (cfg.agentFiles != { }) {
        home.file = lib.mapAttrs' (
          name: source:
          lib.nameValuePair ".cursor/agents/${name}.md" {
            inherit source;
          }
        ) cfg.agentFiles;
      })
    ];
  };
}
