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

    skillsDir = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Local directory of cursor skills linked recursively into ~/.cursor/skills/.";
    };

    skillDirs = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      # The same upstream SKILL.md files the prompt set captures with
      # flake-prompt's skillFromDir; here they go to cursor as they are. The
      # humanizer repo is a whole plugin, so only its SKILL.md is picked out.
      default = {
        caveman = inputs.claude-prompts + /skills/caveman;
        humanizer = pkgs.writeTextDir "SKILL.md" (builtins.readFile (inputs.humanizer + /SKILL.md));
        emacs-skills = inputs.emacs-skills + /skills;
      };
      description = "Skill directories, linked into ~/.cursor/skills/<name>/.";
    };

    agentFiles = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      # Cursor's user-level subagents, linked straight from the source repo:
      # flake-prompt has no cursor backend, so it cannot render this one. The
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

      # Own the cursor half of agent-bundle (skills + mcp.json). Merged with the
      # tools list the pi module sets, so the bundle covers both targets.
      { programs.agent-bundle.tools = [ "cursor" ]; }

      (lib.mkIf (cfg.skillsDir != null) {
        home.file.".cursor/skills" = {
          source = cfg.skillsDir;
          recursive = true;
        };
      })

      (lib.mkIf (cfg.skillDirs != { }) {
        home.file = lib.mapAttrs' (
          name: source:
          lib.nameValuePair ".cursor/skills/${name}" {
            inherit source;
            recursive = true;
          }
        ) cfg.skillDirs;
      })

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
