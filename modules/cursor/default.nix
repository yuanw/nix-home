{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.cursor;
  claudePlugins = pkgs.callPackage ../../packages/claude-plugins { };
in
{
  options.modules.cursor = {
    enable = lib.mkEnableOption "cursor";

    skillsDir = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = ./skills;
      description = "Local directory of cursor rules linked recursively into ~/.cursor/rules/.";
    };

    skillPackages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = with claudePlugins; [
        caveman
        humanizer
        emacs-skills
      ];
      description = "Skill packages built with mkClaudeSkill. Each is linked into ~/.cursor/rules/<pname>/.";
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = lib.mkMerge [
      { home.packages = [ pkgs.llm-agents.cursor-cli ]; }

      (lib.mkIf (cfg.skillsDir != null) {
        home.file.".cursor/rules/_local" = {
          source = cfg.skillsDir;
          recursive = true;
        };
      })

      (lib.mkIf (cfg.skillPackages != [ ]) {
        home.file = lib.listToAttrs (
          map (s: {
            name = ".cursor/rules/${s.passthru.claudeSkill.pname}";
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
