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

      # Cursor-side skills, linked as raw source trees via agent-bundle.
      # Scoped to .cursor/skills: the pi side owns .pi/agent/skills. The
      # cursor module keeps the upstream skills as they are, same shape as
      # agent-bundle's skillFromDir capture for pi.
      {
        programs.agent-bundle = {
          tools = [ "cursor" ];
          skillSources = [
            {
              name = "caveman";
              source = inputs.claude-prompts + /skills/caveman;
              skillDirs = [ ".cursor/skills" ];
            }
            {
              name = "humanizer";
              source = pkgs.writeTextDir "SKILL.md" (builtins.readFile (inputs.humanizer + /SKILL.md));
              skillDirs = [ ".cursor/skills" ];
            }
            {
              name = "emacs-skills";
              source = inputs.emacs-skills + /skills;
              skillDirs = [ ".cursor/skills" ];
            }
          ];
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
