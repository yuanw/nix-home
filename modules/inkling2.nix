# Inkling 2 Mini — served natively on the DGX Spark by TensorFold PR #4441's
# CUDA engine (packages/tensorfold-inkling2: dc2ca8a1 + the
# Inkling-2-Mini-DGX-Spark-TensorFold recipe's patches 0001–0018). One model
# at a time on the Spark's 128 GiB, so this unit Conflicts with
# services.tensorfold-zig (the Qwen3.8-Flash-Next server): starting Inkling
# stops Qwen, and `systemctl start tensorfold-zig` afterwards stops Inkling.
#
# Model: thinkingmachines/Inkling-2-Mini (26.4B MoE, ModelOpt NVFP4/MXFP8,
# pinned revision), downloaded by the unit's preStart into the shared HF cache
# (the same /var/lib/vllm/huggingface the retired Qwen recipe used). Drafts:
# incoai/Inkling-2-Mini-DFlash2 by default (DRAFTER=dflash2, --drafter), or
# the checkpoint's own DSpark head (dspark). Weights ~27 GiB, then 8 full
# 262,144-token windows at fp8 KV (~4.6 GiB a stream) — the engine refuses to
# start with less than TENSORFOLD_MEMORY_RESERVE_GIB free.
#
# The inkling2 and dflash2 "snapshot directories" under the HF cache are what
# `serve` gets as the checkpoint folder: the recipe serves the same files at a
# folder's top level (config.json, model.safetensors.index.json,
# model-00001-of-00001.safetensors, mtp.safetensors, tokenizer files).
#
# The recipe's RERISKED=1 (local Inkling-2-Mini-Derisked) is not modeled:
# point modelId/modelRevision at the Hub copy and set reasoningField = "both"
# to match its deployment kit. Plan: docs/tensorfold-zig-plan.org (the Qwen
# recipe's plan; this unit follows the same shape).
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
    optionalString
    types
    concatStringsSep
    replaceStrings
    ;

  cfg = config.services.inkling2;

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

  # The engine is served the checkpoint's snapshot directory (the recipe's
  # MODEL_DIR flat folder and HF snapshots carry the same top-level files).
  hfDir = repo: "${cfg.hfCache}/hub/models--${replaceStrings [ "/" ] [ "--" ] repo}";
  modelSnapshotDir = "${hfDir cfg.modelId}/snapshots/${cfg.modelRevision}";
  drafterSnapshotDir = "${hfDir cfg.drafterId}/snapshots/${cfg.drafterRevision}";

  serveArgs = [
    "--host ${cfg.host}"
    "--port ${toString cfg.port}"
    "--name ${cfg.servedName}"
    "--context ${toString cfg.context}"
    "--parallel ${toString cfg.parallel}"
    "--max-tokens ${toString cfg.maxTokens}"
    "--temperature ${toString cfg.temperature}"
    "--top-p ${toString cfg.topP}"
    "--top-k ${toString cfg.topK}"
  ]
  ++ optional cfg.thinking "--thinking"
  ++ optional (!cfg.thinking) "--no-thinking"
  ++ optional (cfg.reasoningEffort != null) "--reasoning-effort ${cfg.reasoningEffort}"
  ++ (
    if cfg.drafts then
      [ "--drafter ${if cfg.drafter == "dflash2" then drafterSnapshotDir else modelSnapshotDir}" ]
    else
      [ "--no-drafts" ]
  )
  ++ optional (cfg.kvDtype == "fp8") "--kv-cache fp8"
  ++ optionals cfg.vision (
    [
      "--vision"
    ]
    ++ optional cfg.visionUrls "--vision-urls"
    ++ optional (cfg.visionMaxImages != null) "--vision-max-images ${toString cfg.visionMaxImages}"
    ++ optional (
      cfg.visionImageTokens != null
    ) "--vision-image-tokens ${toString cfg.visionImageTokens}"
  );
in
{
  options.services.inkling2 = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Serve Inkling 2 Mini with TensorFold's Inkling 2 CUDA engine.";
    };

    host = mkOption {
      type = types.str;
      default = "0.0.0.0";
      description = "Bind address for the OpenAI-compatible API.";
    };

    port = mkOption {
      type = types.port;
      default = 8888;
      description = "TCP port for the API (one model at a time, so it shares Qwen's port).";
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
      default = "Inkling-2-Mini";
      description = "Model id clients see in /v1/models and replies.";
    };

    modelId = mkOption {
      type = types.str;
      default = "thinkingmachines/Inkling-2-Mini";
      description = "HuggingFace repo of the checkpoint (≡ the recipe's HF_REPO).";
    };

    modelRevision = mkOption {
      type = types.str;
      default = "7a67ae2314b44ef05c87fd1d4d311c58aaeb18f6";
      description = "Pinned checkpoint revision (≡ scripts/fetch.sh's HF_REVISION).";
    };

    drafterId = mkOption {
      type = types.str;
      default = "incoai/Inkling-2-Mini-DFlash2";
      description = "HuggingFace repo of the DFlash2 block-diffusion draft model (Apache-2.0).";
    };

    drafterRevision = mkOption {
      type = types.str;
      default = "4f9088ea66c21f2b99ed657d1941c306b03e84d2";
      description = "Pinned DFlash2 revision.";
    };

    context = mkOption {
      type = types.ints.positive;
      default = 262144;
      description = "Prompt + reply window a stream (up to the model's 1,048,576).";
    };

    parallel = mkOption {
      type = types.ints.positive;
      default = 8;
      description = "Requests admitted at once (every stream reserves its full window at load).";
    };

    maxTokens = mkOption {
      type = types.ints.positive;
      default = 32768;
      description = "Reply length when a request sets no max_tokens.";
    };

    kvDtype = mkOption {
      type = types.enum [
        "bf16"
        "fp8"
      ];
      default = "fp8";
      description = "Target KV cache precision (fp8 is lossy; bf16 exact — with, say, parallel = 4). The drafters' caches stay bf16.";
    };

    drafts = mkOption {
      type = types.bool;
      default = true;
      description = "Draft (--drafter) instead of --no-drafts.";
    };

    drafter = mkOption {
      type = types.enum [
        "dflash2"
        "dspark"
      ];
      default = "dflash2";
      description = "dflash2: the DFlash2 draft model; dspark: the checkpoint's own DSpark head (mtp.safetensors).";
    };

    thinking = mkOption {
      type = types.bool;
      default = true;
      description = "Serve with a think block by default.";
    };

    reasoningEffort = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Optional default reasoning_effort: none, minimal, low, medium, high, xhigh, max, or a number in [0, 0.99].";
    };

    temperature = mkOption {
      type = types.number;
      default = 1.0;
      description = "Default sampling temperature (a request's own values win).";
    };

    topP = mkOption {
      type = types.number;
      default = 1.0;
      description = "Default top-p.";
    };

    topK = mkOption {
      type = types.ints.unsigned;
      default = 0;
      description = "Default top-k.";
    };

    vision = mkOption {
      type = types.bool;
      default = true;
      description = "Serve the inkling2_vision tower (--vision; PNG/JPEG/WebP, ~0.6 GiB, native CUDA).";
    };

    visionUrls = mkOption {
      type = types.bool;
      default = false;
      description = "Also fetch http(s) image_urls (--vision-urls); data: URLs always work.";
    };

    visionMaxImages = mkOption {
      type = types.nullOr types.ints.positive;
      default = null;
      description = "Images a request may carry (engine default 4, up to 100).";
    };

    visionImageTokens = mkOption {
      type = types.nullOr types.ints.positive;
      default = null;
      description = "An image's token budget (engine default 4,096).";
    };

    attnFp8 = mkOption {
      type = types.bool;
      default = true;
      description = "TF_INKLING2_ATTN_FP8=1: FP8 attention projections (0 for exact bf16).";
    };

    prefillTc = mkOption {
      type = types.bool;
      default = true;
      description = "TF_INKLING2_PREFILL_TC=1: Blackwell native FP4/MXFP8 prefill tensor cores (0 for exact bf16 MMA).";
    };

    reasoningField = mkOption {
      type = types.nullOr (
        types.enum [
          "reasoning_content"
          "reasoning"
          "both"
        ]
      );
      default = null;
      description = "TF_REASONING_FIELD: which field carries Inkling's reasoning. The Derisked kit's vLLM serves a `reasoning` field; 'both' matches it.";
    };

    memoryReserveGib = mkOption {
      type = types.ints.positive;
      default = 9;
      description = "TENSORFOLD_MEMORY_RESERVE_GIB: GiB kept free when sizing the KV and prompt-cache pool.";
    };

    stateDir = mkOption {
      type = types.path;
      default = "/var/lib/tensorfold-inkling2";
      description = "State directory (the engine's HOME / caches).";
    };

    hfCache = mkOption {
      type = types.path;
      default = "/var/lib/vllm/huggingface";
      description = "HuggingFace cache holding the checkpoint and the DFlash2 draft (~20 GiB), the same shared cache the retired Qwen recipe used.";
    };

    environmentFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Environment file loaded for the service (e.g. an agenix HF_TOKEN for the first download; TENSORFOLD_API_KEY can ride along).";
    };
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [ cfg.port ];

    systemd.tmpfiles.rules = [ "d ${cfg.stateDir} 0755 root root - -" ];

    systemd.services.inkling2-mini-tf = {
      description = "TensorFold Inkling 2 Mini server (${cfg.servedName}, native CUDA)";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = optional cfg.autoStart "multi-user.target";
      # 128 GiB of unified memory fits one model. Starting Inkling 2 Mini
      # stops the Qwen3.8-Flash-Next server (services.tensorfold-zig) even if
      # it was left running; a manual `systemctl start tensorfold-zig`
      # afterwards stops this one instead.
      conflicts = [ "tensorfold-zig.service" ];

      path = [
        pkgs.coreutils
        pkgs.bash
      ];

      environment = {
        HF_HOME = cfg.hfCache;
        HF_HUB_OFFLINE = "1";
        LD_LIBRARY_PATH = "/run/opengl-driver/lib:${cudaHome}/lib64:${cudaHome}/lib";
        HOME = cfg.stateDir;
        TENSORFOLD_MEMORY_RESERVE_GIB = toString cfg.memoryReserveGib;
        TF_INKLING2_ATTN_FP8 = if cfg.attnFp8 then "1" else "0";
        TF_INKLING2_PREFILL_TC = if cfg.prefillTc then "1" else "0";
      }
      // optionalAttrs (cfg.reasoningField != null) {
        TF_REASONING_FIELD = cfg.reasoningField;
      };

      # First start: the ~19 GiB checkpoint and, with DFlash2 drafts, the
      # ~1 GiB draft model, resumable, skipped once the pinned snapshots are
      # present. HF_HUB_OFFLINE=1 above is overridden so the downloads can
      # reach the Hub.
      preStart = ''
        set -eu
        snap="${modelSnapshotDir}"
        if [ -d "$snap" ]; then
          echo "checkpoint present: $snap"
        else
          echo "downloading ${cfg.modelId} (~19 GiB) into ${cfg.hfCache}/hub (resumes if interrupted)"
          HF_HUB_OFFLINE=0 ${pkgs.python313Packages.huggingface-hub}/bin/hf download \
            ${cfg.modelId} --revision ${cfg.modelRevision} --cache-dir ${cfg.hfCache}/hub
        fi
        ${optionalString (cfg.drafts && cfg.drafter == "dflash2") ''
          snap="${drafterSnapshotDir}"
          if [ -d "$snap" ]; then
            echo "drafter present: $snap"
          else
            echo "downloading ${cfg.drafterId} (~1 GiB) into ${cfg.hfCache}/hub (resumes if interrupted)"
            HF_HUB_OFFLINE=0 ${pkgs.python313Packages.huggingface-hub}/bin/hf download \
              ${cfg.drafterId} --revision ${cfg.drafterRevision} --cache-dir ${cfg.hfCache}/hub
          fi
        ''}
        test -f "${modelSnapshotDir}/config.json"
      '';

      serviceConfig = {
        Type = "simple";
        WorkingDirectory = cfg.stateDir;
        EnvironmentFile = optional (cfg.environmentFile != null) cfg.environmentFile;
        # systemd word-splits this line, which is the argv the engine expects.
        ExecStart = concatStringsSep " " (
          [
            "${pkgs.tensorfold-inkling2}/native/bin/tensorfold-native"
            "serve"
            modelSnapshotDir
          ]
          ++ serveArgs
        );
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
