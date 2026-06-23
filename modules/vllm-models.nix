{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.vllm-models;
in
{
  options.services.vllm-models = {
    enable = mkEnableOption "declarative HuggingFace model downloads for vLLM";

    cacheDir = mkOption {
      type = types.str;
      default = "/var/lib/vllm/models";
      description = "Directory to store downloaded models.";
    };

    models = mkOption {
      type = types.attrsOf (
        types.submodule (
          { ... }: {
            options = {
              repo = mkOption {
                type = types.str;
                description = "HuggingFace repo ID (e.g. AEON-7/Qwen3.6-27B-AEON-Ultimate-Uncensored-NVFP4).";
              };
              revision = mkOption {
                type = types.nullOr types.str;
                default = null;
                description = "Git revision or branch to pin.";
              };
              extraArgs = mkOption {
                type = types.listOf types.str;
                default = [ ];
                description = "Extra args for huggingface-cli download.";
              };
            };
          }
        )
      );
      default = { };
    };

    # Use huggingface-cli from python3Packages
    package = mkOption {
      type = types.package;
      default = pkgs.python3Packages.huggingface-hub;
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # Ensure cache directory exists
      systemd.tmpfiles.rules = [
        "d ${cfg.cacheDir} 0755 yuanw users - -"
      ];
    }

    {
      # Oneshot services to download each model
      systemd.services = mapAttrs' (
        name: model:
        let
          targetDir = "${cfg.cacheDir}/${name}";
          revArg = optionals (model.revision != null) [
            "--revision"
            model.revision
          ];
        in
        nameValuePair "vllm-model-${name}" {
          description = "Download vLLM model: ${name}";
          after = [ "network.target" ];
          wants = [ "network.target" ];
          wantedBy = [ "multi-user.target" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            User = "yuanw";
            Group = "users";
            EnvironmentFile = optionals (config.age.secrets.hf-token or null != null) [
              config.age.secrets.hf-token.path
            ];
            ExecStart = "${cfg.package}/bin/huggingface-cli download ${model.repo} --local-dir ${targetDir} --local-dir-use-symlinks False ${
              escapeShellArgs (revArg ++ model.extraArgs)
            }";
            TimeoutStartSec = 7200;
          };
        }
      ) cfg.models;
    }

    (mkIf (cfg.models != { } && (config.services.vllm.instances or { }) != { }) {
      # Add model download dependencies to vLLM instances
      systemd.services =
        let
          modelServiceNames = mapAttrsToList (modelName: _: "vllm-model-${modelName}.service") cfg.models;
        in
        mapAttrs' (
          instanceName: _:
          nameValuePair "vllm-${instanceName}" {
            requires = modelServiceNames;
            after = modelServiceNames;
          }
        ) config.services.vllm.instances;
    })
  ]);
}
