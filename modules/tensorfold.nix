# TensorFold serving as a systemd service — native Nix, no container.
#
# Packages TensorFold (packages/tensorfold: upstream + the deployment
# repo's site-packages patch) on the CUDA-enabled nixpkgs torch and runs
# `tensorfold serve` directly, replacing the retired podman recipe (an
# ~11 GB nvcr.io/nvidia/pytorch image that only ever supplied the
# toolchain). TensorFold's ~25 .cu/.cpp kernels are JIT-compiled on
# first start (torch.utils.cpp_extension.load + triton), which is why
# this module wires the JIT toolchain through: nvcc + gcc + ninja +
# bash on the unit's PATH, a merged CUDA tree as CUDA_HOME (the unit's
# `path` option REPLACES the default PATH, so `sh` must be in it —
# ninja runs every rule via `sh -c`), pybind11 headers on CPATH
# (nixpkgs torch does not vendor them into torch/include), and
# cuda_crt/library headers in the merge (headers live in separate
# outputs in the CUDA redist). Kernel caches persist under
# stateDir/kernels so only the first start pays the compile.
{
  config,
  pkgs,
  lib,
  ...
}:

let
  inherit (lib)
    mkIf
    mkEnableOption
    mkOption
    types
    optional
    toString
    replaceStrings
    concatStringsSep
    ;
  cfg = config.services.tensorfold;

  # CUDA_HOME for torch's runtime JIT: cpp_extension wants bin/nvcc,
  # include/cuda_runtime.h and the libraries under one root. Several
  # CUDA redist packages keep their headers in a separate "include"
  # output (libcusparse, libcublas, …), so every output is merged —
  # getDev/getLib alone miss them and the JIT .cu kernels then fail
  # with 'cusparse.h: No such file or directory'.
  getAllOutputs = p: map (o: p.${o}) p.outputs;
  cudaHome = pkgs.symlinkJoin {
    name = "cuda-merged-${pkgs.cudaPackages.cudaMajorMinorVersion}";
    paths = builtins.concatMap getAllOutputs (
      with pkgs.cudaPackages;
      [
        cuda_nvcc
        cuda_crt # include/crt/host_config.h — cuda_runtime.h needs it
        cuda_cudart
        cuda_cccl
        cuda_nvrtc
        cuda_nvtx
        libcublas
        libcurand
        libcusparse
        libcusolver
      ]
    );
  };

  # python313, not the default python3 (3.14): TensorFold self-tests on
  # 3.12/3.13. python313Packages.torch is the CUDA-enabled build (the
  # host sets nixpkgs.config.cudaSupport + cudaCapabilities).
  tfPython = pkgs.python313.withPackages (
    ps:
    [ pkgs.python313Packages.tensorfold ]
    ++ (with ps; [
      triton
      numpy
      huggingface-hub
      tokenizers
      safetensors
      jinja2
      pillow
      av
      transformers
      # The container image shipped torchvision; without it the Qwen-VL
      # image processor falls back to PIL (works, but warns). The
      # processor only resizes/interpolates on host, so the CPU-compiled
      # extension is enough (the standard pairing: CPU torchvision wheel
      # + CUDA torch) — FORCE_CUDA=0 skips torchvision's own CUDA
      # compile entirely.
      (torchvision.overrideAttrs (o: {
        env = (o.env or { }) // {
          FORCE_CUDA = "0";
        };
      }))
    ])
  );

  serveArgs = [
    "--host ${cfg.host}"
    "--port ${toString cfg.port}"
    "--name ${cfg.servedName}"
    "--parallel ${toString cfg.parallel}"
    "--context ${toString cfg.context}"
    "--kv-dtype ${cfg.kvDtype}"
    "--mtp-drafts ${toString cfg.mtpDrafts}"
    "--mtp-confidence ${toString cfg.mtpConfidence}"
    "--temperature ${toString cfg.temperature}"
    "--top-p ${toString cfg.topP}"
    "--top-k ${toString cfg.topK}"
    "--max-tokens ${toString cfg.maxTokens}"
  ]
  ++ optional cfg.thinking "--thinking"
  ++ optional (!cfg.thinking) "--no-thinking"
  ++ optional cfg.pleOnSsd "--ple-on-ssd"
  ++ optionals' cfg.vision [
    "--vision"
    "--vision-max-images ${toString cfg.visionMaxImages}"
  ];

  # lib.optionals, defined here to keep the inherit list short.
  optionals' = cond: list: if cond then list else [ ];

  environment = {
    HOME = cfg.stateDir;
    # Serve only from the local cache; downloadModel (ExecStartPre)
    # overrides this for the first run.
    HF_HUB_OFFLINE = "1";
    HF_HOME = cfg.hfCache;

    # The first start JIT-compiles the kernels; persist them so only
    # the first start pays (the container persisted /cache the same
    # way). A stale lock under torch_extensions/ means a killed
    # build: delete it before starting again.
    TORCH_EXTENSIONS_DIR = "${cfg.stateDir}/kernels/torch_extensions";
    TRITON_CACHE_DIR = "${cfg.stateDir}/kernels/triton";

    # JIT toolchain: CUDA_HOME = merged nvcc + headers + libs (the
    # unit's path above puts bin/nvcc on PATH); LD_LIBRARY_PATH makes
    # the freshly-built .so's libcudart (and libcuda for torch/triton
    # itself) resolvable.
    CUDA_HOME = "${cudaHome}";
    LD_LIBRARY_PATH = "/run/opengl-driver/lib:${cudaHome}/lib64:${cudaHome}/lib";
    # NVIDIA's container torch vendors pybind11 headers into
    # torch/include; nixpkgs torch ships them in the pybind11 python
    # package instead, and cpp_extension doesn't add that include dir —
    # CPATH does (honored by gcc and nvcc's host compile).
    CPATH = "${pkgs.python313Packages.pybind11}/include";

    # TensorFold's own switches (the deployment recipe's defaults):
    # prompt pieces of 2,048 rows while the n-gram tables sit on SSD,
    # vision tower scratch from the system reserve, startup memory
    # reserve (0.6.0+ takes max(4 GiB, RAM/10) otherwise and refuses
    # large admission), prompt-lookup drafts ahead of MTP, and no
    # update check (the package pins the patched release).
    TENSORFOLD_VISION_WORKSPACE_MIB = "0";
    TENSORFOLD_IMAGE_TOKENS = toString cfg.imageTokens;
    TENSORFOLD_VIDEO_TOKENS = toString cfg.videoTokens;
    TENSORFOLD_MTP_COPY = toString cfg.mtpCopy;
    TENSORFOLD_NO_UPDATE_CHECK = "1";
  }
  // (lib.optionalAttrs (cfg.prefillRows != null) {
    TENSORFOLD_PREFILL_ROWS = toString cfg.prefillRows;
  })
  // (lib.optionalAttrs (cfg.memoryReserveGib != null) {
    TENSORFOLD_MEMORY_RESERVE_GIB = toString cfg.memoryReserveGib;
  });
in
{
  options.services.tensorfold = {
    enable = mkEnableOption "TensorFold serving (native, CUDA JIT)";

    host = mkOption {
      type = types.str;
      default = "0.0.0.0";
      description = "Bind address for the OpenAI-compatible API.";
    };

    port = mkOption {
      type = types.port;
      default = 8888;
      description = "TCP port for the OpenAI-compatible API.";
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = "Open the firewall for the API port.";
    };

    autoStart = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Start on boot. Off by default: the first start downloads the
        ~106 GiB checkpoint and JIT-compiles the kernels, and the unit
        is normally managed by hand.
      '';
    };

    modelId = mkOption {
      type = types.str;
      default = "Vontra/Qwen3.8-Flash-Next-MLX-4bit-MTP";
      description = "HuggingFace repo ID of the checkpoint to serve.";
    };

    servedName = mkOption {
      type = types.str;
      default = "Qwen3.8-Flash-Next";
      description = "Model id clients see in /v1/models and replies.";
    };

    parallel = mkOption {
      type = types.ints.positive;
      default = 5;
      description = "Requests decoded together (streams).";
    };

    context = mkOption {
      type = types.ints.positive;
      default = 262144;
      description = "Prompt + reply window per stream.";
    };

    kvDtype = mkOption {
      type = types.enum [
        "bf16"
        "int8"
        "int4"
      ];
      default = "int8";
      description = "KV cache precision.";
    };

    mtpDrafts = mkOption {
      type = types.ints.positive;
      default = 6;
      description = "At most this many MTP drafts a round.";
    };

    mtpConfidence = mkOption {
      type = types.float;
      default = 0.6;
      description = "A chain stops before a later draft under this confidence.";
    };

    temperature = mkOption {
      type = types.float;
      default = 1.0;
      description = "Default sampling temperature (a request's own value wins).";
    };

    topP = mkOption {
      type = types.float;
      default = 0.95;
      description = "Default top-p.";
    };

    topK = mkOption {
      type = types.ints.positive;
      default = 20;
      description = "Default top-k.";
    };

    maxTokens = mkOption {
      type = types.ints.positive;
      default = 32768;
      description = ''
        Reply length for a request that sets no max_tokens. TensorFold's
        own default (4,096) can end a thinking reply before it answers.
      '';
    };

    thinking = mkOption {
      type = types.bool;
      default = true;
      description = "Serve with a think block by default.";
    };

    pleOnSsd = mkOption {
      type = types.bool;
      default = true;
      description = "Read the ~30 GiB n-gram tables from SSD.";
    };

    vision = mkOption {
      type = types.bool;
      default = true;
      description = "Serve the model's own vision tower (image and video input).";
    };

    visionMaxImages = mkOption {
      type = types.ints.positive;
      default = 50;
      description = "Images a request may carry (a chat's turns all count).";
    };

    prefillRows = mkOption {
      type = types.nullOr types.ints.positive;
      default = 2048;
      description = ''
        TENSORFOLD_PREFILL_ROWS override. With the n-gram tables on SSD,
        2,048 measured 10-30% faster than 4,096 on v0.6.1 (5k-16k-token
        prompts); null leaves TensorFold's own choice.
      '';
    };

    memoryReserveGib = mkOption {
      type = types.nullOr types.ints.positive;
      default = 2;
      description = ''
        TENSORFOLD_MEMORY_RESERVE_GIB: GiB left out of MemAvailable at
        startup. TensorFold 0.6.0+ otherwise takes max(4 GiB, a tenth of
        RAM) and refuses 5 x 262,144-token admission; the knob's floor
        is 2 GiB. null: TensorFold's default.
      '';
    };

    mtpCopy = mkOption {
      type = types.ints.unsigned;
      default = 1;
      description = "Prompt-lookup drafts ahead of MTP (0: off).";
    };

    imageTokens = mkOption {
      type = types.ints.positive;
      default = 16384;
      description = "Token budget a request's images share.";
    };

    videoTokens = mkOption {
      type = types.ints.positive;
      default = 16384;
      description = "Token budget for the whole video of a request.";
    };

    user = mkOption {
      type = types.str;
      default = "qwen38";
      description = "User the service runs as (owns the state and caches).";
    };

    group = mkOption {
      type = types.str;
      default = "qwen38";
      description = "Group for the service user.";
    };

    stateDir = mkOption {
      type = types.path;
      default = "/var/lib/qwen38-tensorfold";
      description = ''
        State directory: kernel JIT caches under kernels/, TensorFold's
        own caches under HOME.
      '';
    };

    hfCache = mkOption {
      type = types.path;
      default = "/var/lib/vllm/huggingface";
      description = ''
        HuggingFace cache holding the checkpoint (~106 GiB), shared with
        other serving recipes.
      '';
    };

    environmentFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        Environment file loaded for the service (e.g. an agenix secret
        with HF_TOKEN for the checkpoint download).
      '';
    };
  };

  config = mkIf cfg.enable {
    users.groups.${cfg.group} = { };
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.group;
      extraGroups = [ "video" ]; # GPU device access
      home = cfg.stateDir;
      createHome = false;
      description = "TensorFold serving state owner";
    };

    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [ cfg.port ];

    systemd.tmpfiles.rules = [
      "d ${cfg.stateDir} 0755 ${cfg.user} ${cfg.group} - -"
      "d ${cfg.stateDir}/kernels 0755 ${cfg.user} ${cfg.group} - -"
      "d ${cfg.hfCache} 0755 ${cfg.user} ${cfg.group} - -"
    ];

    systemd.services.tensorfold = {
      description = "TensorFold server (${cfg.servedName}, native CUDA)";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = optional cfg.autoStart "multi-user.target";

      # nvcc (first-start JIT of TensorFold's CUDA kernels — without it
      # torch's cpp_extension fails to build the .so) + gcc (cpp_extension
      # compiles the host side with `c++`, which must be on PATH or ninja
      # dies with "posix_spawn: No such file or directory"; torch itself
      # was built with g++, so use nixpkgs gcc, not clang) + bash (ninja
      # spawns every rule via `sh -c`, and the unit's PATH replaces the
      # default, so sh must be in it explicitly) + ninja (the JIT build
      # system) + git (huggingface_hub's git checks).
      path = with pkgs; [
        cudaHome
        gcc
        ninja
        bash
        gitMinimal
        coreutils
      ];

      inherit environment;

      # The checkpoint download from the deployment recipe (scripts/
      # prepare.sh ran `hf download ... --cache-dir $HF_CACHE/hub`,
      # resumable): skipped once a snapshot is present. HF_HUB_OFFLINE=1
      # from the unit environment is overridden here so the first run
      # can actually reach the Hub.
      preStart = ''
        set -eu
        modelDir="${cfg.hfCache}/hub/models--${replaceStrings [ "/" ] [ "--" ] cfg.modelId}"
        if [ -n "$(ls -d "$modelDir"/snapshots/*/ 2>/dev/null)" ]; then
          echo "checkpoint present: $modelDir"
          exit 0
        fi
        echo "downloading ${cfg.modelId} (~106 GiB) into ${cfg.hfCache}/hub (resumes if interrupted)"
        HF_HUB_OFFLINE=0 ${pkgs.python313Packages.huggingface-hub}/bin/hf download ${cfg.modelId} --cache-dir ${cfg.hfCache}/hub
      '';

      serviceConfig = {
        Type = "simple";
        WorkingDirectory = cfg.stateDir;
        User = cfg.user;
        Group = cfg.group;
        EnvironmentFile = optional (cfg.environmentFile != null) cfg.environmentFile;
        # systemd word-splits this line, which is exactly the argv
        # tensorfold expects; a request's own sampling values win over
        # the defaults above.
        ExecStart = concatStringsSep " " [
          "${tfPython}/bin/tensorfold"
          "serve"
          cfg.modelId
          (concatStringsSep " " serveArgs)
        ];
        # The container ran with --ulimit memlock=-1 --ulimit stack=67108864.
        LimitMEMLOCK = "infinity";
        LimitSTACK = 67108864;
        # The first start downloads the ~106 GiB checkpoint (hours on a
        # LAN uplink) and JIT-compiles the kernels, so no start timeout:
        # the unit is in "activating" until preStart/ExecStart run.
        TimeoutStartSec = 0;
        TimeoutStopSec = 120;
        # SIGINT (not SIGTERM) so tensorfold can drain streams cleanly.
        KillSignal = "SIGINT";
      };
    };
  };
}
