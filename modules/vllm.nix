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

      # ─── Backend ───
      backend = mkOption {
        type = types.enum [
          "native"
          "podman"
        ];
        default = "native";
        description = ''
          Run the native vllm-aeon binary (native) or the AEON OCI image via
          podman + CDI (podman). For backend=podman the host `model` and
          speculative drafter paths are bind-mounted to /model and /drafter
          inside the container, so the serve args use those container paths.
        '';
      };

      containerImage = mkOption {
        type = types.str;
        default = "ghcr.io/aeon-7/aeon-vllm-ultimate:latest";
        description = ''
          OCI image used when backend=podman. Pin by digest
          (image@sha256:...) for reproducibility once a known-good build is
          validated. The AEON image ships vLLM 0.23.0 for GB10 / sm_121a
          with the qwen3_5_moe arch + DFlash pre-compiled.
        '';
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

      mambaCacheDtype = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          GatedDeltaNet (SSM) recurrent state precision for hybrid models
          (--mamba-cache-dtype, e.g. "float32" for Qwen3.6 / Ornith).
        '';
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

      # Resolve paths: native uses host paths; podman uses container mount
      # points (bind-mounted below), so serve args reference /model and /drafter.
      modelArg = if inst.backend == "podman" then "/model" else inst.model;
      drafterArg = if inst.backend == "podman" then "/drafter" else inst.speculative.model;

      vllmArgs = [
        "serve"
        modelArg
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
      ++ optionals (inst.mambaCacheDtype != null) [
        "--mamba-cache-dtype"
        inst.mambaCacheDtype
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
        ''{"method":"dflash","model":"${drafterArg}","num_speculative_tokens":${toString inst.speculative.numSpeculativeTokens}}''
      ]
      ++ inst.extraArgs;

      envDefaults = {
        HF_HOME = "/var/lib/vllm/huggingface";
      }
      // optionalAttrs (config.age.secrets.hf-token or null != null) {
        HF_TOKEN_PATH = config.age.secrets.hf-token.path;
      };

      # ── native backend ──
      vllmBin = "${pkgs.vllm-aeon or pkgs.vllm}/bin/vllm";
      nativeExecStart = "${vllmBin} ${escapeShellArgs vllmArgs}";

      # ── podman backend ──
      # Container env: drop HF_TOKEN_PATH (secret lives on host; the model is
      # already bind-mounted so serve-time auth is usually unneeded), and
      # remap HF_HOME to the in-container mount point.
      containerEnv = removeAttrs (envDefaults // inst.extraEnv) [ "HF_TOKEN_PATH" ] // {
        HF_HOME = "/root/.cache/huggingface";
      };
      containerMounts = [
        "${inst.model}:/model:ro"
      ]
      ++ optionals inst.speculative.enable [ "${inst.speculative.model}:/drafter:ro" ]
      ++ [ "/var/lib/vllm/huggingface:/root/.cache/huggingface" ];
      podmanArgs = [
        "run"
        "--rm"
        "--name"
        "vllm-${name}"
        "--device"
        "nvidia.com/gpu=all" # CDI (not --gpus all); matches graham33 GB10 playbooks
        "--ipc"
        "host"
        "--network"
        "host"
      ]
      ++ concatLists (
        mapAttrsToList (k: v: [
          "-e"
          "${k}=${v}"
        ]) containerEnv
      )
      ++ concatMap (m: [
        "-v"
        m
      ]) containerMounts
      ++ [
        "--entrypoint"
        "vllm"
        inst.containerImage
      ]
      ++ vllmArgs;
      podmanExecStart = "${pkgs.podman}/bin/podman ${escapeShellArgs podmanArgs}";

      # Image present + named container cleared before each start (Restart
      # tolerance: a --rm container may linger after an unclean kill).
      podmanExecStartPre = [
        "+${pkgs.bash}/bin/bash -c 'sync; echo 3 > /proc/sys/vm/drop_caches'"
        # CDI spec is what makes --device nvidia.com/gpu=all work. Mirrors
        # graham33's playbook shellHook guard.
        "+${pkgs.bash}/bin/bash -c 'test -f /etc/cdi/nvidia.yaml -o -f /var/run/cdi/nvidia-container-toolkit.json || { echo \"CDI spec missing — run: sudo nvidia-ctk cdi generate --output=/etc/cdi/nvidia.yaml\"; exit 1; }'"
        "${pkgs.podman}/bin/podman pull ${escapeShellArg inst.containerImage}"
        "${pkgs.bash}/bin/bash -c '${pkgs.podman}/bin/podman rm -f vllm-${name} || true'"
      ];

      execStart = if inst.backend == "podman" then podmanExecStart else nativeExecStart;
      execStartPre =
        if inst.backend == "podman" then
          podmanExecStartPre
        else
          [
            "+${pkgs.bash}/bin/bash -c 'sync; echo 3 > /proc/sys/vm/drop_caches'"
          ];
    in
    {
      description = "vLLM inference server (${name}: ${inst.model})";
      after = [ "network.target" ];
      wantedBy = mkIf inst.autoStart [ "multi-user.target" ];
      requires = optionals inst.autoStart [ "network.target" ];
      # Ensure only one vLLM instance runs at a time — they share the GPU.
      conflicts = map (n: "vllm-${n}.service") otherNames;

      serviceConfig = {
        ExecStartPre = execStartPre;
        ExecStart = execStart;
        Restart = "on-failure";
        RestartSec = 10;

        # Environment (native backend reads these directly; podman backend
        # receives them via -e from containerEnv).
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

    # Risk R4 (QUICKSTART §4): on the DGX Spark, DFlash's per-sequence
    # speculative-verify buffers are not fully counted by
    # --gpu-memory-utilization; at max-num-seqs > ~16 they exhaust the 121 GB
    # unified pool and hard-crash the box (kernel NVRM NV_ERR_NO_MEMORY).
    # Guard at eval time so a misconfigured instance fails to activate
    # instead of taking down the host.
    assertions = flatten (
      mapAttrsToList (
        name: inst:
        optionals inst.speculative.enable [
          {
            assertion = inst.maxNumSeqs <= 16;
            message =
              "vllm instance `${name}` has DFlash speculative decoding enabled "
              + "with max-num-seqs=${toString inst.maxNumSeqs} > 16; this hard-crashes "
              + "the DGX Spark unified-memory pool (see QUICKSTART §4). Cap at 16 "
              + "or disable speculative decoding.";
          }
        ]
      ) enabledInstances
    );
  };
}
