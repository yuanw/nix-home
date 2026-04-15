{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.cursor;
  claudePlugins = pkgs.callPackage ../../../packages/claude-plugins { };
in
{
  options.modules.cursor = {
    enable = lib.mkEnableOption "cursor";

    skillsDir = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Local directory of cursor skills linked recursively into ~/.cursor/skills/.";
    };

    skillPackages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = with claudePlugins; [
        caveman
        humanizer
        emacs-skills
      ];
      description = "Skill packages built with mkClaudeSkill. Each is linked into ~/.cursor/skills/<pname>/.";
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = lib.mkMerge [
      { home.packages = [ (lib.lowPrio pkgs.llm-agents.cursor-agent) ]; }

      (lib.mkIf (cfg.skillsDir != null) {
        home.file.".cursor/skills" = {
          source = cfg.skillsDir;
          recursive = true;
        };
      })

      (lib.mkIf (cfg.skillPackages != [ ]) {
        home.file = lib.listToAttrs (
          map (s: {
            name = ".cursor/skills/${s.passthru.claudeSkill.pname}";
            value = {
              source = s;
              recursive = true;
            };
          }) cfg.skillPackages
        );
      })
    ];
  };
}
