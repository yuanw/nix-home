# tensorfold-native — TensorFold's Zig CUDA engine, packaged natively
# (packages/tensorfold-zig). Serves Qwen3.8 Flash Next (INT4-AutoRound) on
# :8890, beside the Python TensorFold (:8888) and RED-SNOW (:8899); one
# model at a time on 128 GB unified memory.
#
# Unlike the Python TensorFold there is no first-start JIT: the CUDA fatbins
# and the Triton AOT cubins are baked into the package, so the unit only
# downloads the checkpoint (once) and loads the weights.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkIf
    mkOption
    optional
    types
    concatStringsSep
    replaceStrings
    ;

  cfg = config.services.tensorfold-zig;

  # nvcc + CUDA runtime/CCCL headers + the libs the binary loads at runtime.
  cudaHome = pkgs.symlinkJoin {
    name = "cuda-merged-${pkgs.cudaPackages.cudaMajorMinorVersion}";
    paths = with pkgs.cudaPackages; [
      cuda_nvcc
      cuda_crt
      cuda_cudart
      cuda_cccl
      cuda_nvrtc
      cuda_nvtx
      libcublas
      libcurand
      libcusparse
      libcusolver
    ];
  };

  # The engine is served the checkpoint's snapshot directory, as the
  # recipe's start.sh does (MODEL_ARG).
  snapshotDir = "${cfg.hfCache}/hub/models--${
    replaceStrings [ "/" ] [ "--" ] cfg.modelId
  }/snapshots/${cfg.modelRevision}";

  serveArgs = [
    "--host ${cfg.host}"
    "--port ${toString cfg.port}"
    "--name ${cfg.servedName}"
    "--context ${toString cfg.context}"
    "--parallel ${toString cfg.parallel}"
    "--max-tokens ${toString cfg.maxTokens}"
    "--kv-dtype ${cfg.kvDtype}"
  ]
  ++ optional cfg.thinking "--thinking"
  ++ optional (!cfg.thinking) "--no-thinking";
in
{
  options.services.tensorfold-zig = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Serve Qwen3.8 Flash Next with TensorFold's Zig engine.";
    };

    host = mkOption {
      type = types.str;
      default = "0.0.0.0";
      description = "Bind address for the OpenAI-compatible API.";
    };

    port = mkOption {
      type = types.port;
      default = 8890;
      description = "TCP port for the API (8888 is the Python TensorFold, 8899 RED-SNOW).";
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = "Open the firewall for the API port.";
    };

    autoStart = mkOption {
      type = types.bool;
      default = false;
      description = "Start on boot. Off: one model at a time on the Spark.";
    };

    servedName = mkOption {
      type = types.str;
      default = "Qwen3.8-Flash-Next";
      description = "Model id clients see in /v1/models and replies.";
    };

    modelId = mkOption {
      type = types.str;
      default = "azampatti/Qwen3.8-Flash-Next-125B-A5B-INT4-AutoRound";
      description = "HuggingFace repo of the checkpoint the Zig engine serves.";
    };

    modelRevision = mkOption {
      type = types.str;
      default = "1464274120d36a4d8fcaa934552334a7d83ce0fd";
      description = "Pinned checkpoint revision (the snapshot directory).";
    };

    context = mkOption {
      type = types.ints.positive;
      default = 262144;
      description = "Prompt + reply window per request.";
    };

    parallel = mkOption {
      type = types.ints.positive;
      default = 8;
      description = "Requests admitted at once (the engine refuses ones that do not fit).";
    };

    kvDtype = mkOption {
      type = types.enum [
        "bf16"
        "fp8"
      ];
      default = "fp8";
      description = "KV cache precision (fp8 is lossy; bf16 is exact).";
    };

    maxTokens = mkOption {
      type = types.ints.positive;
      default = 32768;
      description = "Reply length when a request sets no max_tokens.";
    };

    thinking = mkOption {
      type = types.bool;
      default = true;
      description = "Serve with a think block by default.";
    };

    memoryReserveGib = mkOption {
      type = types.ints.positive;
      default = 10;
      description = "TENSORFOLD_MEMORY_RESERVE_GIB: GiB kept free when sizing the KV pool.";
    };

    stateDir = mkOption {
      type = types.path;
      default = "/var/lib/tensorfold-zig";
      description = "State directory (the engine's HOME / caches).";
    };

    hfCache = mkOption {
      type = types.path;
      default = "/var/lib/vllm/huggingface";
      description = "HuggingFace cache holding the checkpoint (~122 GiB), shared with the other recipes.";
    };

    environmentFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Environment file loaded for the service (e.g. an agenix HF_TOKEN for the first download).";
    };
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [ cfg.port ];

    systemd.tmpfiles.rules = [ "d ${cfg.stateDir} 0755 root root - -" ];

    systemd.services.tensorfold-zig = {
      description = "TensorFold Zig server (${cfg.servedName}, native CUDA)";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = optional cfg.autoStart "multi-user.target";

      path = [
        pkgs.coreutils
        pkgs.bash
      ];

      environment = {
        TENSORFOLD_CUDA_KERNELS = "${pkgs.tensorfold-zig}/share/tensorfold/cuda/sm121";
        HF_HOME = cfg.hfCache;
        HF_HUB_OFFLINE = "1";
        LD_LIBRARY_PATH = "/run/opengl-driver/lib:${cudaHome}/lib64:${cudaHome}/lib";
        HOME = cfg.stateDir;
        TENSORFOLD_MEMORY_RESERVE_GIB = toString cfg.memoryReserveGib;
      };

      # First start: the ~122 GiB checkpoint, resumable, skipped once the
      # pinned snapshot is present. HF_HUB_OFFLINE=1 above is overridden so
      # the download can reach the Hub.
      preStart = ''
        set -eu
        modelDir="${cfg.hfCache}/hub/models--${replaceStrings [ "/" ] [ "--" ] cfg.modelId}"
        if [ -d "${snapshotDir}" ]; then
          echo "checkpoint present: ${snapshotDir}"
          exit 0
        fi
        echo "downloading ${cfg.modelId} (~122 GiB) into ${cfg.hfCache}/hub (resumes if interrupted)"
        HF_HUB_OFFLINE=0 ${pkgs.python313Packages.huggingface-hub}/bin/hf download \
          ${cfg.modelId} --revision ${cfg.modelRevision} --cache-dir ${cfg.hfCache}/hub
      '';

      serviceConfig = {
        Type = "simple";
        WorkingDirectory = cfg.stateDir;
        EnvironmentFile = optional (cfg.environmentFile != null) cfg.environmentFile;
        # systemd word-splits this line, which is the argv the engine expects.
        ExecStart = concatStringsSep " " [
          "${pkgs.tensorfold-zig}/bin/tensorfold-native"
          "serve"
          snapshotDir
          (concatStringsSep " " serveArgs)
        ];
        LimitMEMLOCK = "infinity";
        LimitSTACK = 67108864;
        # The first start downloads the checkpoint; no start timeout.
        TimeoutStartSec = 0;
        TimeoutStopSec = 120;
        KillSignal = "SIGINT";
      };
    };
  };
}
