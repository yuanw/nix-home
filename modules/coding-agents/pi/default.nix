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
  agentConfigPath = "${home}/${agentConfigDir}";
in
{
  options.modules.pi = {
    enable = lib.mkEnableOption ''
      Pi coding agent via nixpi (programs.pi).

      This module enables nixpi Home Manager integration and small nix-home
      glue (extension modules, permission-gate rules, mics-skills). Host knobs
      (defaultProvider, extensions, rawSkills, …) are declared here and passed
      through to programs.pi.
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

    # Legacy: still used by nix-home-private modules/work.nix.
    # Prefer modules.pi.rawSkills for new code.
    skills = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      description = ''
        Legacy pi-only skill packages linked under .pi/agent/skills/<pname>.
        Prefer modules.pi.rawSkills.
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
      hm@{ ... }:
      let
        permissionGateEnabled = cfg.extensions.permission-gate.enable or false;
        skillFiles = lib.listToAttrs (
          map (
            skill:
            lib.nameValuePair "${agentConfigDir}/skills/${skill.pname}" {
              source = skill;
            }
          ) cfg.skills
        );
      in
      {
        imports =
          (import ./local-extensions.nix { inherit inputs pkgs; })
          ++ (import ./third-party-extensions.nix {
            inherit
              inputs
              pkgs
              lib
              ;
          });

        programs.mics-skills.skillDirs = [
          "${agentConfigDir}/skills"
        ];

        programs.pi = {
          enable = true;
          package = lib.mkDefault pkgs.llm-agents.pi;
          environment.variables = {
            PI_TELEMETRY = lib.mkDefault "0";
            PI_CODING_AGENT_DIR = agentConfigPath;
          }
          // cfg.environment;
          settings = lib.filterAttrs (_: v: v != null) {
            defaultProvider = cfg.defaultProvider;
            defaultModel = cfg.defaultModel;
          };
          extensions = cfg.extensions;
          rawSkills = cfg.rawSkills;
          providers =
            (lib.optionalAttrs cfg.localModel {
              dgx-spark = import ./providers/dgx-spark.nix {
                inherit inputs pkgs;
              };
            })
            # Runtime-registered providers must be present for defaultProvider assertions.
            //
              lib.optionalAttrs
                (cfg.defaultProvider != null && !(cfg.localModel && cfg.defaultProvider == "dgx-spark"))
                {
                  ${cfg.defaultProvider} = {
                    enable = true;
                  };
                };
        };

        home.file =
          skillFiles
          // lib.optionalAttrs permissionGateEnabled {
            ".config/pi-agent-extensions/permission-gate/rules.ts".source =
              hm.config.lib.file.mkOutOfStoreSymlink "${home}/${workspace}/nix-home/modules/coding-agents/pi/permission-gate-rules.ts";
          };
      };
  };
}
