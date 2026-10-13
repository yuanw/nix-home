{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.pi;
  user = config.my.username;
  home = config.my.homeDirectory;
  workspace = config.my.workspaceDirectory;
  agentConfigDir = ".pi/agent";

  # Where each programs.pi.extensions.<name> comes from. pi-flake packages
  # whatever is not already a Pi package, so a bare .ts and a shipped package
  # are declared the same way.
  piExtensionPackages = {
    custom-footer = ./extensions/custom-footer.ts;
    direnv = ./extensions/direnv.ts;
    notify = ./extensions/notify.ts;
    web-fetch = ./extensions/web-fetch.ts;
    cursor-agent = pkgs.pi-extensions.pi-cursor-agent;
    interactive-shell = pkgs.pi-extensions.pi-interactive-shell;
    permission-gate = pkgs.pi-extensions.pi-permission-gate;
    ponytail = pkgs.pi-extensions.pi-ponytail;
    slow-mode = pkgs.pi-extensions.pi-slow-mode;
  };
  agentConfigPath = "${home}/${agentConfigDir}";
in
{
  options.modules.pi = {
    enable = lib.mkEnableOption ''
      Pi coding agent via pi-flake (programs.pi).

      This module enables the pi-flake Home Manager integration and small
      nix-home glue (permission-gate rules, mics-skills). Host knobs
      (defaultProvider, extensions, mcpServers, rawSkills, …) are declared here
      and passed through to programs.pi.
    '';

    # Escape hatch for other nix-darwin modules (e.g. browser-cli) that must
    # not write home-manager.users while also reading it.
    environment = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = ''
        Extra programs.pi.environment.variables. Prefer setting
        programs.pi.environment.variables in Home Manager when possible.
      '';
    };

    localModel = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Register the local DGX Spark / TensorFold OpenAI-compatible endpoint as
        programs.pi.providers.dgx-spark (see providers/dgx-spark.nix).
      '';
    };

    defaultProvider = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "cursor-agent";
      description = "Passed to programs.pi.settings.defaultProvider.";
    };

    defaultModel = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "default";
      description = "Passed to programs.pi.settings.defaultModel.";
    };

    extensions = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule {
          freeformType = lib.types.attrsOf lib.types.anything;
          options.enable = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Whether to enable this Pi extension.";
          };
        }
      );
      default = { };
      example = lib.literalExpression ''
        {
          # defaults already enable notify, custom-footer, slow-mode,
          # permission-gate, interactive-shell, and ponytail
          cursor-agent.enable = true;
          web-fetch.enable = true;
        }
      '';
      description = ''
        Passed to programs.pi.extensions. By default enables notify,
        custom-footer, slow-mode, permission-gate, interactive-shell, and
        ponytail. Set `<name>.enable = false` to opt out, or enable extras
        such as cursor-agent / web-fetch / direnv.
      '';
    };

    mcpServers = lib.mkOption {
      type = lib.types.attrsOf (lib.types.attrsOf lib.types.anything);
      default = { };
      example = lib.literalExpression ''
        {
          Amplitude = {
            type = "http";
            url = "https://mcp.amplitude.com/mcp";
          };
        }
      '';
      description = ''
        Passed to programs.pi.mcpServers, which writes it as the
        `mcpServers` key of `${agentConfigDir}/mcp.json`. When empty the file
        is left alone (hand-managed); when non-empty this module owns the file.
      '';
    };

    rawSkills = lib.mkOption {
      type = lib.types.listOf (
        lib.types.oneOf [
          lib.types.package
          lib.types.path
          lib.types.str
        ]
      );
      default = [ ];
      example = lib.literalExpression ''
        [ pkgs.pi-extensions.pi-interactive-shell ]
      '';
      description = "Passed to programs.pi.rawSkills.";
    };
  };

  config = lib.mkIf cfg.enable {
    modules.pi.extensions = {
      notify.enable = lib.mkDefault true;
      custom-footer.enable = lib.mkDefault true;
      slow-mode.enable = lib.mkDefault true;
      permission-gate.enable = lib.mkDefault true;
      interactive-shell.enable = lib.mkDefault true;
      ponytail.enable = lib.mkDefault true;
    };

    home-manager.users.${user} =
      {
        lib,
        ...
      }@hm:
      let
        permissionGateEnabled = cfg.extensions.permission-gate.enable or false;
        micsSkillNames = [
          "browser-cli"
          "kagi-search"
          "pexpect-cli"
          "screenshot-cli"
        ];
      in
      {
        programs.mics-skills = {
          enable = true;
          package = inputs.mics-skills.packages.${pkgs.stdenv.hostPlatform.system};
          skills = micsSkillNames;
          skillDirs = [
            "${agentConfigDir}/skills"
          ];
        };

        # Old modules.pi.skills linked whole store packages as
        # ~/.pi/agent/skills/<name> -> /nix/store/... . pi-flake keeps skills in
        # the store and lists them in settings.json, so those links are stale.
        # Keep mics-skills whole-dir links; remove other store dir symlinks.
        home.activation.removeLegacyAgentPmSkillDirLinks = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
          skillsDir="$HOME/${agentConfigDir}/skills"
          if [ -d "$skillsDir" ]; then
            for path in "$skillsDir"/*; do
              [ -L "$path" ] || continue
              name="''${path##*/}"
              case "$name" in
                ${lib.concatMapStringsSep "|" lib.escapeShellArg micsSkillNames}) continue ;;
              esac
              target="$(readlink "$path" || true)"
              case "$target" in
                /nix/store/*) run rm -f "$path" ;;
              esac
            done
          fi
        '';

        programs.pi = {
          enable = true;
          package = lib.mkDefault pkgs.llm-agents.pi;

          # Skills, slash commands and AGENTS.md, from the shared prompt set.
          flakePrompt.prompts = import ../prompts {
            inherit pkgs lib;
          };

          mcpServers = cfg.mcpServers;
          environment.variables = {
            PI_TELEMETRY = lib.mkDefault "0";
            PI_CODING_AGENT_DIR = agentConfigPath;
          }
          // cfg.environment;
          settings = lib.filterAttrs (_: v: v != null) {
            defaultProvider = cfg.defaultProvider;
            defaultModel = cfg.defaultModel;
          };
          extensions = lib.mapAttrs (
            name: ext:
            ext
            // {
              package = ext.package or piExtensionPackages.${name} or null;
            }
          ) cfg.extensions;

          rawSkills = cfg.rawSkills;
          # Only declare providers that have real models.json fields here.
          # Extension-registered ones (e.g. cursor-agent via pi-cursor-agent)
          # must not get an empty stub — Pi rejects `providers.<name>: {}` with
          # "must specify baseUrl, headers, compat, modelOverrides, or models".
          providers = lib.optionalAttrs cfg.localModel {
            dgx-spark = import ./providers/dgx-spark.nix;
          };
        };

        home.file = lib.optionalAttrs permissionGateEnabled {
          ".config/pi-agent-extensions/permission-gate/rules.ts".source =
            hm.config.lib.file.mkOutOfStoreSymlink "${home}/${workspace}/nix-home/modules/coding-agents/pi/permission-gate-rules.ts";
        };
      };
  };
}
