# tensorfold-native — TensorFold's Zig CUDA engine, packaged natively
# (packages/tensorfold-zig). Serves Qwen3.8 Flash Next from NVIDIA's FP4 QAD
# checkpoint (nvidia/Qwen3.8-Flash-Next-FP4-Local) on :8888, beside RED-SNOW
# (:8899); one model at a time on 128 GB unified memory. The recipe's earlier
# INT4-AutoRound checkpoint still works: override modelId + modelRevision.
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
    optionalAttrs
    optionals
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

  # The --vision helper's interpreter: Pillow + PyAV + transformers (5.17.0)
  # + the CUDA torch, running TensorFold's own Python tree.
  visionPython = pkgs.python313Packages.python.withPackages (ps: [
    ps.torch
    ps.transformers
    ps.av
    ps.pillow
  ]);

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
  ++ optional (!cfg.thinking) "--no-thinking"
  ++ optionals cfg.vision [
    "--vision"
    "--vision-max-images ${toString cfg.visionMaxImages}"
    "--vision-max-videos ${toString cfg.visionMaxVideos}"
    "--vision-image-tokens ${toString cfg.visionImageTokens}"
  ];
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
      default = 8888;
      description = "TCP port for the API (the Python TensorFold used the same port, so either engine serves :8888).";
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
      default = "nvidia/Qwen3.8-Flash-Next-FP4-Local";
      description = "HuggingFace repo of the checkpoint the Zig engine serves (the recipe's INT4-AutoRound is azampatti/Qwen3.8-Flash-Next-125B-A5B-INT4-AutoRound).";
    };

    modelRevision = mkOption {
      type = types.str;
      default = "b9ce18a6887e18c089fdea8509a33e63429a3300";
      description = "Pinned checkpoint revision (the snapshot directory).";
    };

    context = mkOption {
      type = types.ints.positive;
      default = 262144;
      description = "Prompt + reply window per request.";
    };

    parallel = mkOption {
      type = types.ints.positive;
      default = 5;
      description = "Requests admitted at once (the engine refuses ones that do not fit). 5 fills NVIDIA-FP4's ~24 GiB fp8 pool with full windows.";
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

    vision = mkOption {
      type = types.bool;
      default = true;
      description = "Serve the model's vision tower (image/video input) via a Python helper.";
    };

    visionMaxImages = mkOption {
      type = types.ints.positive;
      default = 50;
      description = "Images a request may carry.";
    };

    visionMaxVideos = mkOption {
      type = types.ints.positive;
      default = 4;
      description = "Videos a request may carry.";
    };

    visionImageTokens = mkOption {
      type = types.ints.positive;
      default = 16384;
      description = "Token budget a request's images share.";
    };

    visionWorkspaceMib = mkOption {
      type = types.ints.positive;
      default = 2048;
      description = "TENSORFOLD_VISION_WORKSPACE_MIB: MiB kept for the vision helper.";
    };

    memoryReserveGib = mkOption {
      type = types.ints.positive;
      default = 10;
      description = "TENSORFOLD_MEMORY_RESERVE_GIB: GiB kept free when sizing the KV pool.";
    };

    systemMemoryCache = mkOption {
      type = types.bool;
      default = false;
      description = ''
        TF_FLASHNEXT_SYSTEM_CACHE=1: register ATS-locked system-memory base-page
        caches/state and use native VMM rings (recipe patch 0010, issue #33's
        NV_ERR_NO_MEMORY mitigation). Needs the native VMM (single device).
      '';
    };

    ringCache = mkOption {
      type = types.bool;
      default = false;
      description = "TF_FLASHNEXT_RING_CACHE=1: reuse native VMM ring regions (needs systemMemoryCache).";
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
        # The recipe's FP4 tuning (scripts/config.sh): 6 MTP drafts a round and
        # the product stop rule up to 5 streams; the engine's own defaults are
        # 15/2. On INT4 these are the wrong trade (INT4's numbers predate them).
        TF_FLASHNEXT_DEPTH = "6";
        TF_FLASHNEXT_PRODUCT_STREAMS = "5";
      }
      // optionalAttrs cfg.vision {
        # --vision runs a helper process (python3 -m tensorfold.vision.native_helper)
        # with TensorFold's Python tree and a Pillow/PyAV/transformers/torch env.
        TENSORFOLD_VISION_PYTHON = "${visionPython}/bin/python3";
        TENSORFOLD_VISION_PYTHONPATH = "${pkgs.tensorfold-zig}/share/tensorfold/python";
        TENSORFOLD_VISION_WORKSPACE_MIB = toString cfg.visionWorkspaceMib;
      }
      // optionalAttrs cfg.systemMemoryCache {
        TF_FLASHNEXT_SYSTEM_CACHE = "1";
      }
      // optionalAttrs cfg.ringCache {
        TF_FLASHNEXT_RING_CACHE = "1";
      };

      # First start: the ~96 GiB checkpoint, resumable, skipped once the
      # pinned snapshot is present. HF_HUB_OFFLINE=1 above is overridden so
      # the download can reach the Hub.
      preStart = ''
        set -eu
        modelDir="${cfg.hfCache}/hub/models--${replaceStrings [ "/" ] [ "--" ] cfg.modelId}"
        if [ -d "${snapshotDir}" ]; then
          echo "checkpoint present: ${snapshotDir}"
          exit 0
        fi
        echo "downloading ${cfg.modelId} (~96 GiB) into ${cfg.hfCache}/hub (resumes if interrupted)"
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
