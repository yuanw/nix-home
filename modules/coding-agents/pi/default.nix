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
      glue (extension modules, permission-gate rules, mics-skills). Configure
      Pi itself with programs.pi under home-manager.users, e.g.:

        home-manager.users.''${username}.programs.pi = {
          settings.defaultProvider = "cursor-agent";
          extensions.notify.enable = true;
          extensions.cursor-agent.enable = true;
        };
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
    # Prefer programs.pi.rawSkills for new code.
    skills = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      description = ''
        Legacy pi-only skill packages linked under .pi/agent/skills/<pname>.
        Prefer programs.pi.rawSkills.
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
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user} =
      hm@{ config, ... }:
      let
        permissionGateEnabled = config.programs.pi.extensions.permission-gate.enable or false;
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

        # nixpi-native configuration surface. Hosts extend programs.pi.*.
        programs.pi = {
          enable = true;
          package = lib.mkDefault pkgs.llm-agents.pi;
          environment.variables = {
            PI_TELEMETRY = lib.mkDefault "0";
            PI_CODING_AGENT_DIR = agentConfigPath;
          }
          // cfg.environment;
          providers = lib.optionalAttrs cfg.localModel {
            dgx-spark = import ./providers/dgx-spark.nix {
              inherit inputs pkgs;
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
