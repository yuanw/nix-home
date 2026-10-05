{
  config,
  lib,
  pkgs,

  ...
}:
let
  cfg = config.modules.pi;
  hasPermissionGate = lib.any (p: p.pname == "permission-gate") cfg.extensionsPkgs;
  defaultConfigDir = ".pi/agent";
  agentConfigPath = "${config.my.homeDirectory}/${cfg.configDir}";
  agentPmManagedSkillNames = [
    "caveman"
    "d2"
    "denote-note"
    "describe"
    "disk-space"
    "dired"
    "emacs-skills"
    "emacsclient"
    "explain-diff-html"
    "file-links"
    "gnuplot"
    "grilling"
    "highlight"
    "humanizer"
    "i-have-adhd"
    "journal-session"
    "mermaid"
    "open"
    "open-code-review-delegate"
    "org-agenda-todo"
    "plantuml"
    "ponytail"
    "ponytail-audit"
    "ponytail-debt"
    "ponytail-gain"
    "ponytail-help"
    "ponytail-review"
    "select"
    "teach"
  ];
  agentPmManagedPromptNames = [
    "journal-session.md"
  ];
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

  # Always install extensions under <configDir>/extensions/ so Pi's jiti loader
  # resolves @mariozechner/* / typebox against the agent runtime. Putting bare
  # store paths in programs.pi.rawExtensions (settings.json) loads them outside
  # that context and fails with "Cannot find package ...".
  extensionHomeFiles =
    lib.listToAttrs (mkEntries cfg.extensionsPkgs (ext: "extensions/${ext.pname}") (x: x))
    // lib.mapAttrs' (
      name: path: lib.nameValuePair "${cfg.configDir}/extensions/${name}" { source = path; }
    ) cfg.extensionFiles;

  sharedHomeFiles =
    hm:
    extensionHomeFiles
    // lib.listToAttrs (
      (mkEntries cfg.skills (skill: "skills/${skill.pname}") (x: x))
      ++ (mkEntries (lib.attrsToList cfg.themes) (t: "themes/${t.name}.json") (t: t.value.src))
    )
    // lib.optionalAttrs (cfg.nodeDeps != null) {
      "${cfg.configDir}/node_modules".source = "${cfg.nodeDeps}/node_modules";
    }
    // lib.optionalAttrs (cfg.skillsDir != null) {
      "${cfg.configDir}/skills".source = cfg.skillsDir;
    }
    // lib.mapAttrs' (
      name: path: lib.nameValuePair "${cfg.configDir}/prompts/${name}" { source = path; }
    ) cfg.prompts
    // lib.optionalAttrs hasPermissionGate {
      ".config/pi-agent-extensions/permission-gate/rules.ts".source =
        hm.config.lib.file.mkOutOfStoreSymlink "${config.my.homeDirectory}/${config.my.workspaceDirectory}/nix-home/modules/coding-agents/pi/permission-gate-rules.ts";
    };
in
{
  options.modules.pi = {
    enable = lib.mkEnableOption "pi";

    enableWorkMux = lib.mkEnableOption "workmux";

    useNixpi = lib.mkEnableOption ''
      Drive Pi through the nixpi Home Manager module (programs.pi).

      When enabled, nixpi owns the wrapped `pi` package and generated
      settings.json. Extensions stay as home-manager links under
      <configDir>/extensions/ (not programs.pi.rawExtensions store paths —
      those break package resolution). This module still owns models.json via
      mergetools (nixpi's provider→models.json path drops freeform fields like
      `compat`), legacy skills/prompts/themes links, permission-gate rules, and
      agent-pm coexistence. Sets PI_CODING_AGENT_DIR to the HM-managed config
      dir so the nixpi launcher does not redirect to XDG. Only enable on hosts
      that import nixpi.homeModules.default.
    '';

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
        Each package's pname is used as the filename under
        <configDir>/extensions/.
      '';
    };

    extensionFiles = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = { };
      example = lib.literalExpression ''
        {
          "notify.ts" = ./extensions/notify.ts;
          "my-tool.ts" = ./extensions/my-tool.ts;
        }
      '';
      description = ''
        Local .ts files to link directly into <configDir>/extensions/.
        Keys are the filenames (must include .ts suffix); values are paths to
        the source files. No packaging step required.
        Multi-file extensions belong in extensionsPkgs (see pi-permission-gate).
      '';
    };

    skills = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      description = ''
        Legacy pi-only skill packages. Shared skills should be declared in
        modules/coding-agents/prompts and rendered through programs.agent-pm
        instead. Each package's pname is used as the skill directory name under
        <configDir>/skills/.
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
      default = { };
      description = ''
        Legacy pi-only prompt templates to install under <configDir>/prompts/.
        Shared prompts should be declared in modules/coding-agents/prompts and
        rendered through programs.agent-pm instead. Keys are filenames (must
        include .md suffix); values are paths to the template files.
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

    models = lib.mkOption {
      type = lib.types.attrsOf lib.types.anything;
      default = { };
      example = lib.literalExpression ''
        {
          providers = {
            ollama = {
              api = "openai-completions";
              baseUrl = "http://localhost:11434/v1";
              models = [ { id = "llama3"; } ];
            };
          };
        }
      '';
      description = ''
        Declarative pi model configuration. Merged into <configDir>/models.json
        at activation time using yq, preserving existing settings and backing up
        the previous file. Set to { } to skip model management.

        Still used when useNixpi is enabled: nixpi's generated models.json omits
        freeform provider fields (e.g. compat) needed by custom providers.
      '';
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
      {
        assertion = !cfg.useNixpi || cfg.configDir == defaultConfigDir;
        message = ''
          modules.pi.useNixpi requires configDir = "${defaultConfigDir}" because
          nixpi's Home Manager integration hardcodes .pi/agent for settings.json.
        '';
      }
      (mkNoDuplicateAssertion (map (p: p.pname) cfg.extensionsPkgs) "extension")
      (mkNoDuplicateAssertion (map (s: s.pname) cfg.skills) "skill")
      {
        assertion = lib.intersectLists (map (s: s.pname) cfg.skills) agentPmManagedSkillNames == [ ];
        message = ''
          These pi skills are managed by programs.agent-pm, not modules.pi.skills:
          ${lib.concatStringsSep ", " agentPmManagedSkillNames}
        '';
      }
      {
        assertion = lib.intersectLists (lib.attrNames cfg.prompts) agentPmManagedPromptNames == [ ];
        message = ''
          These pi prompts are managed by programs.agent-pm, not modules.pi.prompts:
          ${lib.concatStringsSep ", " agentPmManagedPromptNames}
        '';
      }
    ];

    home-manager.users.${config.my.username} =
      hm@{ ... }:
      lib.mkMerge [
        {
          programs.mics-skills.skillDirs = [
            "${cfg.configDir}/skills"
          ];

          home.packages = lib.optional cfg.enableWorkMux pkgs.llm-agents.workmux;

          home.file = sharedHomeFiles hm;

          mergetools = lib.mkIf (cfg.models != { }) {
            "pi-models" = {
              target = "${agentConfigPath}/models.json";
              format = "json";
              force = true;
              settings = cfg.models;
            };
          };
        }

        (lib.mkIf cfg.useNixpi {
          # nixpi owns the wrapped package + settings.json; pin the agent dir so
          # the launcher uses HM-managed files (extension links + mergetools
          # models + agent-pm skills) instead of XDG_DATA_HOME/nixpi/agent.
          # Leave rawExtensions empty: Pi discovers ~/.pi/agent/extensions/.
          programs.pi = {
            enable = true;
            package = cfg.package;
            environment.variables = cfg.environment // {
              PI_CODING_AGENT_DIR = agentConfigPath;
            };
          };
        })

        (lib.mkIf (!cfg.useNixpi) {
          home.packages = [ cfg.package ];

          home.sessionVariables =
            cfg.environment
            // lib.optionalAttrs (cfg.configDir != defaultConfigDir) {
              PI_CODING_AGENT_DIR = "$HOME/${cfg.configDir}";
            };
        })
      ];
  };
}
