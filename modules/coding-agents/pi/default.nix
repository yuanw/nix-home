{
  config,
  lib,
  pkgs,

  ...
}:
let
  cfg = config.modules.pi;
  defaultConfigDir = ".pi/agent";
  claudePlugins = pkgs.callPackage ../../../packages/claude-plugins { };
  commonPrompts = pkgs.callPackage ../common/prompts.nix { };
  mkEntries =
    items: nameOf: sourceOf:
    map (item: lib.nameValuePair "${cfg.configDir}/${nameOf item}" { source = (sourceOf item); }) items;

  mkNoDuplicateAssertion =
    values: entityKind:
    let
      duplicates = lib.filter (value: lib.count (x: x == value) values > 1) (lib.unique values);
      mkMsg = value: "  - ${entityKind} `${toString value}`";
    in
    {
      assertion = duplicates == [ ];
      message = ''
        Must not have duplicate ${entityKind}s:
      ''
      + lib.concatStringsSep "\n" (map mkMsg duplicates);
    };
in
{
  options.modules.pi = {
    enable = lib.mkEnableOption "pi";

    enableWorkMux = lib.mkEnableOption "workmux";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.llm-agents.pi;
    };

    configDir = lib.mkOption {
      type = lib.types.str;
      default = defaultConfigDir;
      example = ".config/pi/agent";
      description = ''
        Directory where pi agent files (extensions, skills, themes) are stored,
        relative to the home directory.
      '';
    };

    environment = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = {
        PI_TELEMETRY = "0";
      };
      example = lib.literalExpression ''
        {
          PI_SKIP_VERSION_CHECK = "1";
        }
      '';
      description = "Extra environment variables to set for pi.";
    };

    nodeDeps = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      example = lib.literalExpression ''
        pkgs.callPackage ./pi-node-deps.nix { }
      '';
      description = ''
        A derivation whose node_modules/ directory is linked into
        <configDir>/node_modules/. Build it with buildNpmPackage +
        importNpmLock. Required when extensions import runtime npm packages
        (anything beyond `import type` from the pi API).
      '';
    };

    extensionsPkgs = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      description = ''
        Pi extension packages built with mkPiExtension or mkLocalPiExtension.
        Each package's pname is used as the filename under <configDir>/extensions/.
      '';
    };

    extensionFiles = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = { };
      example = lib.literalExpression ''
        {
          "permission-gate.ts" = ./extensions/permission-gate.ts;
          "my-tool.ts" = ./extensions/my-tool.ts;
        }
      '';
      description = ''
        Local .ts files to link directly into <configDir>/extensions/.
        Keys are the filenames (must include .ts suffix); values are paths to
        the source files. No packaging step required.
      '';
    };

    skills = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = with claudePlugins; [
        caveman
        humanizer
        emacs-skills
      ];
      description = ''
        Pi skill packages. Each package's pname is used as the skill directory
        name under <configDir>/skills/.
        name under <configDir>/skills/.
      '';
    };

    skillsDir = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = ''
        Path to a directory containing skill files to link into <configDir>/skills/.
        Set to null to disable.
      '';
    };

    prompts = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = {
        "journal-session.md" = commonPrompts.mkJournalSessionPrompt "pi";
      };
      description = ''
        Prompt templates to install under <configDir>/prompts/.
        Keys are filenames (must include .md suffix); values are paths to the template files.
      '';
    };

    themes = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule {
          options.src = lib.mkOption {
            type = lib.types.path;
            description = "Path to the theme JSON file.";
          };
        }
      );
      default = { };
      description = "Custom themes to install under <configDir>/themes/.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !(cfg.environment ? PI_CODING_AGENT_DIR);
        message = ''
          modules.pi.environment.PI_CODING_AGENT_DIR is managed by modules.pi.configDir.
          Set modules.pi.configDir instead of PI_CODING_AGENT_DIR.
        '';
      }
      (mkNoDuplicateAssertion (map (p: p.pname) cfg.extensionsPkgs) "extension")
      (mkNoDuplicateAssertion (map (s: s.pname) cfg.skills) "skill")
    ];

    home-manager.users.${config.my.username} = {
      home.packages = [ cfg.package ] ++ lib.optional cfg.enableWorkMux pkgs.llm-agents.workmux;

      home.file =
        lib.listToAttrs (
          (mkEntries cfg.extensionsPkgs (ext: "extensions/${ext.pname}") (x: x))
          ++ (mkEntries cfg.skills (skill: "skills/${skill.pname}") (x: x))
          ++ (mkEntries (lib.attrsToList cfg.themes) (t: "themes/${t.name}.json") (t: t.value.src))
        )
        // lib.mapAttrs' (
          name: path: lib.nameValuePair "${cfg.configDir}/extensions/${name}" { source = path; }
        ) cfg.extensionFiles
        // lib.optionalAttrs (cfg.nodeDeps != null) {
          "${cfg.configDir}/node_modules".source = "${cfg.nodeDeps}/node_modules";
        }
        // lib.optionalAttrs (cfg.skillsDir != null) {
          "${cfg.configDir}/skills".source = cfg.skillsDir;
        }
        // lib.mapAttrs' (
          name: path: lib.nameValuePair "${cfg.configDir}/prompts/${name}" { source = path; }
        ) cfg.prompts;

      home.sessionVariables =
        cfg.environment
        // lib.optionalAttrs (cfg.configDir != defaultConfigDir) {
          PI_CODING_AGENT_DIR = "$HOME/${cfg.configDir}";
        };
    };
  };
}
