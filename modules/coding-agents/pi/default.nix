{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.pi;
  hasPermissionGate = lib.any (p: p.pname == "permission-gate") cfg.extensionsPkgs;
  defaultConfigDir = ".pi/agent";
  agentConfigPath = "${config.my.homeDirectory}/${cfg.configDir}";
  toNixpiPackage = import ./to-nixpi-package.nix {
    inherit lib pkgs;
    mkPiExtension = inputs.nixpi.lib.nixpi.mkPiExtension;
  };
  nixpiPackages =
    (map toNixpiPackage.fromExtensionPkg cfg.extensionsPkgs)
    ++ lib.mapAttrsToList toNixpiPackage.fromExtensionFile cfg.extensionFiles;
  # Runtime-registered providers (cursor-agent, …) must appear in
  # programs.pi.providers for settings.defaultProvider assertions.
  defaultProviderName = cfg.settings.defaultProvider or null;
  nixpiProviders =
    cfg.providers
    // lib.optionalAttrs (defaultProviderName != null && !(cfg.providers ? ${defaultProviderName})) {
      ${defaultProviderName} = {
        enable = true;
      };
    };
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

  sharedHomeFiles =
    hm:
    lib.listToAttrs (
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
    enable = lib.mkEnableOption ''
      Pi coding agent via nixpi (programs.pi).

      modules.pi is the host-facing API; nixpi owns the wrapped `pi` package,
      settings.json, models.json, and extension packages. This module still
      owns legacy skills/prompts/themes links, permission-gate rules, and
      agent-pm coexistence. Requires nixpi.homeModules.default in HM
      sharedModules.
    '';

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
        Directory where pi agent files (skills, themes) are stored, relative
        to the home directory. Must stay at the default because nixpi's Home
        Manager integration hardcodes .pi/agent for settings.json.
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
        Pi extension packages. Each is converted to a Pi package and added to
        programs.pi.packages.
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
        Local .ts files wrapped as Pi packages in programs.pi.packages.
        Keys should include the .ts suffix.
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

    settings = lib.mkOption {
      type = lib.types.attrsOf lib.types.anything;
      default = { };
      example = lib.literalExpression ''
        {
          defaultProvider = "cursor-agent";
          defaultModel = "default";
        }
      '';
      description = ''
        Values merged into programs.pi.settings (and thus settings.json).
      '';
    };

    providers = lib.mkOption {
      type = lib.types.attrsOf lib.types.attrs;
      default = { };
      example = lib.literalExpression ''
        {
          cursor-agent.enable = true;
        }
      '';
      description = ''
        programs.pi.providers declarations. Use
        inputs.nixpi.lib.nixpi.mkPiProvider (see providers/dgx-spark.nix) for
        static OpenAI-compatible endpoints, or `{ enable = true; }` for
        runtime-registered providers (e.g. cursor-agent). Freeform fields such
        as `compat` are preserved in models.json by nixpi.
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
        assertion = cfg.configDir == defaultConfigDir;
        message = ''
          modules.pi.configDir must be "${defaultConfigDir}" because nixpi's
          Home Manager integration hardcodes .pi/agent for settings.json.
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
      {
        programs.mics-skills.skillDirs = [
          "${cfg.configDir}/skills"
        ];

        home.packages = lib.optional cfg.enableWorkMux pkgs.llm-agents.workmux;

        home.file = sharedHomeFiles hm;

        # nixpi backend. Host knobs stay on modules.pi.*.
        programs.pi = {
          enable = true;
          package = cfg.package;
          packages = nixpiPackages;
          settings = cfg.settings;
          providers = nixpiProviders;
          environment.variables = cfg.environment // {
            PI_CODING_AGENT_DIR = agentConfigPath;
          };
        };
      };
  };
}
