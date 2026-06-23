{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.vllm;

  instanceModule = { ... }: {
    options = {
      enable = mkEnableOption "this vLLM instance" // {
        default = true;
      };

      model = mkOption {
        type = types.str;
        description = "HuggingFace model ID or local path to serve.";
        example = "/var/lib/vllm/models/Qwen3.6-27B-AEON-NVFP4";
      };

      servedModelName = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Name to expose via the API (--served-model-name).";
      };

      port = mkOption {
        type = types.port;
        default = 8000;
        description = "Port for the OpenAI-compatible API server.";
      };

      host = mkOption {
        type = types.str;
        default = "0.0.0.0";
        description = "Host address to bind to.";
      };

      autoStart = mkOption {
        type = types.bool;
        default = false;
        description = "Start this instance automatically on boot.";
      };

      # ─── DGX Spark specific ───
      gpuMemoryUtilization = mkOption {
        type = types.float;
        default = 0.78;
        description = ''
          Fraction of GPU memory to use for model weights and KV cache.
          On DGX Spark, never exceed 0.88 — unified LPDDR5X thrashes above that.
        '';
      };

      maxModelLen = mkOption {
        type = types.int;
        default = 24576;
        description = "Maximum context length (tokens).";
      };

      maxNumSeqs = mkOption {
        type = types.int;
        default = 8;
        description = "Maximum number of concurrent sequences.";
      };

      maxNumBatchedTokens = mkOption {
        type = types.nullOr types.int;
        default = null;
        description = "Maximum number of batched tokens (--max-num-batched-tokens).";
      };

      # ─── Quantization ───
      quantization = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Quantization method (e.g. compressed-tensors for NVFP4).";
      };

      dtype = mkOption {
        type = types.str;
        default = "auto";
        description = "Data type for model weights (--dtype).";
      };

      kvCacheDtype = mkOption {
        type = types.nullOr types.str;
        default = "fp8_e4m3";
        description = ''
          KV cache data type. "fp8_e4m3" for FP8, "nvfp4" for NVFP4 (needs PR #44389).
        '';
      };

      # ─── Speculative decoding (DFlash) ───
      speculative = {
        enable = mkEnableOption "DFlash speculative decoding";
        model = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = "Path to DFlash drafter model.";
        };
        numSpeculativeTokens = mkOption {
          type = types.int;
          default = 12;
          description = "Number of tokens to speculate (--num-speculative-tokens).";
        };
      };

      # ─── Optimizations ───
      enforceEager = mkOption {
        type = types.bool;
        default = false;
        description = "Disable CUDA graph compilation (--enforce-eager).";
      };

      enableChunkedPrefill = mkOption {
        type = types.bool;
        default = true;
        description = "Enable chunked prefill (--enable-chunked-prefill).";
      };

      enablePrefixCaching = mkOption {
        type = types.bool;
        default = true;
        description = "Enable automatic prefix caching (--enable-prefix-caching).";
      };

      mambaBlockSize = mkOption {
        type = types.nullOr types.int;
        default = null;
        description = "Mamba block size for hybrid models (--mamba-block-size, e.g. 256 for Qwen3.6).";
      };

      toolCallParser = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Tool/function calling parser (e.g. qwen3_coder).";
      };

      reasoningParser = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Reasoning/thinking parser (e.g. qwen3, deepseek_r1).";
      };

      extraArgs = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Additional command-line arguments for vllm serve.";
      };

      extraEnv = mkOption {
        type = types.attrsOf types.str;
        default = { };
        description = "Extra environment variables for the service.";
      };
    };
  };

  enabledInstances = filterAttrs (_: inst: inst.enable) cfg.instances;
  instanceNames = attrNames enabledInstances;

  mkService =
    name: inst:
    let
      otherNames = filter (n: n != name) instanceNames;

      args = [
        "serve"
        inst.model
        "--host"
        inst.host
        "--port"
        (toString inst.port)
        "--dtype"
        inst.dtype
        "--gpu-memory-utilization"
        (toString inst.gpuMemoryUtilization)
        "--max-model-len"
        (toString inst.maxModelLen)
        "--max-num-seqs"
        (toString inst.maxNumSeqs)
      ]
      ++ optionals (inst.servedModelName != null) [
        "--served-model-name"
        inst.servedModelName
      ]
      ++ optionals (inst.quantization != null) [
        "--quantization"
        inst.quantization
      ]
      ++ optionals (inst.kvCacheDtype != null) [
        "--kv-cache-dtype"
        inst.kvCacheDtype
      ]
      ++ optionals (inst.maxNumBatchedTokens != null) [
        "--max-num-batched-tokens"
        (toString inst.maxNumBatchedTokens)
      ]
      ++ optionals inst.enforceEager [ "--enforce-eager" ]
      ++ optionals inst.enableChunkedPrefill [ "--enable-chunked-prefill" ]
      ++ optionals inst.enablePrefixCaching [ "--enable-prefix-caching" ]
      ++ optionals (inst.mambaBlockSize != null) [
        "--mamba-block-size"
        (toString inst.mambaBlockSize)
      ]
      ++ optionals (inst.toolCallParser != null) [
        "--enable-auto-tool-choice"
        "--tool-call-parser"
        inst.toolCallParser
      ]
      ++ optionals (inst.reasoningParser != null) [
        "--reasoning-parser"
        inst.reasoningParser
      ]
      ++ optionals inst.speculative.enable [
        "--speculative-config"
        ''{"method":"dflash","model":"${inst.speculative.model}","num_speculative_tokens":${toString inst.speculative.numSpeculativeTokens}}''
      ]
      ++ optionals inst.enablePrefixCaching [ "--enable-prefix-caching" ]
      ++ inst.extraArgs;

      envDefaults = {
        HF_HOME = "/var/lib/vllm/huggingface";
      }
      // optionalAttrs (config.age.secrets.hf-token or null != null) {
        HF_TOKEN_PATH = config.age.secrets.hf-token.path;
      };
    in
    {
      description = "vLLM inference server (${name}: ${inst.model})";
      after = [ "network.target" ];
      wantedBy = mkIf inst.autoStart [ "multi-user.target" ];
      requires = optionals inst.autoStart [ "network.target" ];
      # Ensure only one vLLM instance runs at a time — they share the GPU.
      conflicts = map (n: "vllm-${n}.service") otherNames;

      serviceConfig = {
        # Drop page caches so vLLM sees the full unified memory pool.
        ExecStartPre = [
          "+${pkgs.bash}/bin/bash -c 'sync; echo 3 > /proc/sys/vm/drop_caches'"
        ];
        ExecStart = "${pkgs.vllm-aeon or pkgs.vllm}/bin/vllm ${escapeShellArgs args}";
        Restart = "on-failure";
        RestartSec = 10;

        # Environment
        Environment = mapAttrsToList (k: v: "${k}=${v}") (envDefaults // inst.extraEnv);
        EnvironmentFile = optionals (config.age.secrets.hf-token or null != null) [
          config.age.secrets.hf-token.path
        ];
      };
    };
in
{
  options.services.vllm = {
    instances = mkOption {
      type = types.attrsOf (types.submodule instanceModule);
      default = { };
      description = ''
        Named vLLM inference server instances. Each instance becomes a
        systemd service `vllm-<name>.service`. Instances declare mutual
        conflicts so only one can run at a time on a single-GPU host.
      '';
    };
  };

  config = mkIf (enabledInstances != { }) {
    systemd.services = mapAttrs' (
      name: inst: nameValuePair "vllm-${name}" (mkService name inst)
    ) enabledInstances;

    # Create cache directory for HuggingFace downloads
    systemd.tmpfiles.rules = [
      "d /var/lib/vllm/huggingface 0755 yuanw users - -"
      "d /var/lib/vllm/models 0755 yuanw users - -"
    ];
  };
}
