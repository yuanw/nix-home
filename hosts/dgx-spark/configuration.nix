{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  # ─── Bootloader ─────────────────────────────────────────────────────
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.configurationLimit = 10;

  # ─── DGX Spark hardware ─────────────────────────────────────────────
  # Enabling this upstream module (inputs.dgx-spark.nixosModules.dgx-[
  # spark) ALSO turns on the OCI inference runtime this host uses for the
  # AEON vLLM container (services.vllm.instances.* with backend = "podman"):
  # virtualisation.podman.{enable,dockerCompat,dockerSocket} +
  # networking.firewall.trustedInterfaces = [ "podman+" ] and
  # hardware.nvidia-container-toolkit.enable (which generates the CDI spec
  # consumed by `podman run --device nvidia.com/gpu=all`). See graham33
  # modules/dgx-spark.nix:164-174 and plans/ornith-dgx-spark-docker.org.
  hardware.dgx-spark.enable = true;

  # ─── Networking ─────────────────────────────────────────────────────
  networking.hostName = "dgx-spark";
  networking.useDHCP = true;

  # ─── Time zone / locale ─────────────────────────────────────────────
  time.timeZone = "America/Regina";
  i18n.defaultLocale = "en_US.UTF-8";

  # ─── User accounts ──────────────────────────────────────────────────
  users.groups.qwen38 = { };
  users.users = {
    yuanw = {
      isNormalUser = true;
      extraGroups = [
        "wheel"
        "video"
        "docker"
      ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHUg80LmE2cirl2gPfmShkWZh68eIvlD6Uc3swGfcAwY me@yuanwang.ca"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFpYgmWwtRG7vlRbtWheYrtHl9E9qx84sdU+YlE8w+CZ me@yuanwang.ca"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHUg80LmE2cirl2gPfmShkWZh68eIvlD6Uc3swGfcAwY me@yuanwang.ca"
      ];
    };

    # Owns the TensorFold state (kernel JIT caches, TF's own caches under
    # HOME) and the service runs as this user: the native unit needs only
    # GPU device access, which the video group provides (the podman recipe
    # ran its container as root because podman needs the rootful engine).
    qwen38 = {
      isSystemUser = true;
      group = "qwen38";
      extraGroups = [ "video" ];
      home = "/var/lib/qwen38-tensorfold";
      createHome = false;
      description = "Qwen3.8 Flash Next TensorFold state owner";
    };
  };

  # ─── Sudo ────────────────────────────────────────────────────────
  security.sudo.wheelNeedsPassword = false;

  # ─── Nix settings ──────────────────────────────────────────────────
  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    auto-optimise-store = true;
    trusted-users = [
      "root"
      "yuanw"
    ];
    substituters = [
      "https://nix-community.cachix.org"
      "https://cuda-maintainers.cachix.org"
      "https://cache.nixos-cuda.org"
      "https://graham33.cachix.org"
      "https://ai.cachix.org"
    ];
    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "cuda-maintainers.cachix.org-1:Zf5/D7lVH62pV3W4pAzbXFPAtdKBKAZnNj4n1XS85i4="
      "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
      "graham33.cachix.org-1:DqH72VpwSrACa3+L9eqh4bixjWx9IQUaxQtRh4gtkX8="
      "ai.cachix.org-1:N9dzRK+alWwoKXQlnn0H6aUx0lU/mspIoz8hMvGvbbc="
    ];
  };
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.cudaSupport = true;
  # vLLM 0.16 has known CVEs (fixed in 0.23). Permit until vllm-aeon builds.
  nixpkgs.config.permittedInsecurePackages = [
    "python3.13-vllm-0.16.0"
  ];

  # ─── DS4 Server ─────────────────────────────────────────────────────
  services.ds4.enable = false;

  # ─── Cockpit Web Manager ────────────────────────────────────────────
  services.cockpit-local.enable = true;
  services.cockpit-local.enableGpu = true;

  # ─── PCP (Performance Co-Pilot) — required by cockpit
  services.pcp.enable = true;

  # ─── Lance Multimodal AI ────────────────────────────────────────────
  services.lance = {
    enable = false;
    instances = {
      video = {
        enable = true;
        model = "video";
        gradioTask = "t2v";
        gradioPort = 7860;
      };
      image = {
        enable = false;
        model = "image";
        gradioTask = "t2i";
        gradioPort = 7861;
      };
    };
  };

  # ─── Firewall ───────────────────────────────────────────────────────
  # 8888 = TensorFold's API port: the native server binds 0.0.0.0, so the
  # API is only reachable while this port is open. 8000/11000/8188 were dropped with this branch:
  # the vLLM service (8000) is gone, the DGX dashboard (11000/11001) is
  # disabled below, and no ComfyUI instance (8188) is configured.
  networking.firewall.allowedTCPPorts = [
    8888 # TensorFold qwen38 (Qwen3.8 Flash Next TensorFold recipe)
  ];

  # ─── TensorFold Inference (native Nix, no container) ───────────────
  # Serves Vontra/Qwen3.8-Flash-Next-MLX-4bit-MTP through TensorFold
  # v0.3.6.3 built natively: 5 streams x 262,144 tokens, int8 KV, vision
  # tower. This replaces the retired podman recipe (clone of
  # yuanw/Qwen3.8-Flash-Next-Single-DGX-Spark-TensorFold + the ~11 GB
  # nvcr.io/nvidia/pytorch:26.07-py3 image): the container only supplied
  # the toolchain (CUDA 13 + torch + triton + nvcc), and TensorFold's own
  # ~25 .cu/.cpp kernels are JIT-compiled on first start anyway
  # (src/tensorfold/cuda/build.py → torch.utils.cpp_extension.load), so
  # the native side replicates it with the CUDA-enabled nixpkgs torch
  # (nixpkgs.config.cudaSupport + cudaCapabilities 12.0/12.1, set in
  # hosts/default.nix) plus nvcc + ninja on this unit's PATH. The nine
  # site-packages patches the image baked in are applied by
  # packages/tensorfold instead; serve args below mirror the retired
  # start.sh defaults (scripts/config.sh). Same serving knobs the
  # container had, so output should be token-for-token identical.
  # First start (downloading the ~106 GiB checkpoint + JIT kernels can
  # take hours, hence the unlimited start timeout):
  #   systemctl start vllm-qwen38-tensorfold   # then: journalctl -fu vllm-qwen38-tensorfold
  # tools/bench.py and tools/needle.py from the deployment repo (or the
  # old repo checkout in /var/lib/qwen38-tensorfold/repo, which this unit
  # no longer manages) still work against the native API for regression
  # comparison with the container numbers (62/90/107/119 tok/s at 1/2/4/5
  # streams).
  systemd.services.vllm-qwen38-tensorfold =
    let
      stateDir = "/var/lib/qwen38-tensorfold";
      kernelsDir = "${stateDir}/kernels";
      qwen38User = "qwen38";
      qwen38Group = "qwen38";
      modelId = "Vontra/Qwen3.8-Flash-Next-MLX-4bit-MTP";
      hfCache = "/var/lib/vllm/huggingface";

      # CUDA_HOME for torch's runtime JIT (cpp_extension needs bin/nvcc,
      # include/cuda_runtime.h and the libraries under one root). Same
      # merged list as packages/vllm-aeon.nix, plus nvcc itself. Note:
      # several CUDA redist packages keep their headers in a separate
      # "include" output (libcusparse, libcublas, …), so every output is
      # merged — getDev/getLib alone miss them and the JIT .cu kernels
      # then fail with 'cusparse.h: No such file or directory'.
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

      # python313, not the default python3 (3.14): TensorFold self-tests
      # on 3.12/3.13. python313Packages.torch here is the CUDA-enabled
      # build (see hosts/default.nix cudaCapabilities). The tensorfold
      # package itself propagates torch/triton/numpy/… (pyproject omits
      # them by design — "CUDA uses the container's torch and triton" —
      # so packages/tensorfold pins them); they are listed here as well
      # to make the serving env independent of that choice.
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

      # The checkpoint download from the retired container recipe
      # (scripts/prepare.sh ran `hf download ... --cache-dir $HF_CACHE/hub`,
      # resumable): skipped once a snapshot is present. HF_HUB_OFFLINE=1
      # from the unit environment is overridden here so the first run can
      # actually reach the Hub.
      downloadModel = pkgs.writeShellScript "qwen38-model-download" ''
        set -eu
        modelDir="${hfCache}/hub/models--${pkgs.lib.replaceStrings [ "/" ] [ "--" ] modelId}"
        if [ -n "$(ls -d "$modelDir"/snapshots/*/ 2>/dev/null)" ]; then
          echo "checkpoint present: $modelDir"
          exit 0
        fi
        echo "downloading ${modelId} (~106 GiB) into ${hfCache}/hub (resumes if interrupted)"
        HF_HUB_OFFLINE=0 ${pkgs.python313Packages.huggingface-hub}/bin/hf download ${modelId} --cache-dir ${hfCache}/hub
      '';
    in
    {
      description = "TensorFold Qwen3.8 Flash Next server (native, single DGX Spark)";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = [ ]; # start manually: systemctl start vllm-qwen38-tensorfold

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

      environment = {
        HOME = stateDir;

        # Same HF cache the container recipe mounted (and the retired
        # prepare.sh downloaded into): $HF_HOME/hub holds the ~106 GiB
        # checkpoint, shared across serving recipes.
        HF_HOME = hfCache;
        # Serve only from the local cache (the container default).
        # downloadModel (ExecStartPre) overrides this for the first run.
        HF_HUB_OFFLINE = "1";

        # The first start JIT-compiles the kernels; persist them so only
        # the first start pays (the container persisted /cache the same
        # way). A stale lock under torch_extensions/ means a killed
        # build: delete it before starting again.
        TORCH_EXTENSIONS_DIR = "${kernelsDir}/torch_extensions";
        TRITON_CACHE_DIR = "${kernelsDir}/triton";

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

        # TensorFold's own switches — the exact values the container
        # recipe exported (scripts/config.sh): vision tower with 2,048-
        # row prompt chunks (with --vision), prompt-lookup drafts ahead
        # of MTP, and no update check (the patches pin v0.3.6.3).
        TENSORFOLD_PREFILL_ROWS = "2048";
        TENSORFOLD_VISION_WORKSPACE_MIB = "0";
        TENSORFOLD_MAX_IMAGES = "50";
        TENSORFOLD_IMAGE_TOKENS = "16384";
        TENSORFOLD_VIDEO_TOKENS = "16384";
        TENSORFOLD_MTP_COPY = "1";
        TENSORFOLD_NO_UPDATE_CHECK = "1";
      };

      serviceConfig = {
        Type = "simple";
        WorkingDirectory = stateDir;
        User = qwen38User;
        Group = qwen38Group;
        EnvironmentFile = [ config.age.secrets.hf-token.path ];
        ExecStartPre = downloadModel;
        # SERVE_ARGS of the retired start.sh (scripts/config.sh defaults,
        # THINKING=1, VISION=1, VISION_URLS=0): a request's own sampling
        # values still win over these defaults. (systemd word-splits this
        # line, which is exactly the argv tensorfold expects.)
        ExecStart =
          "${tfPython}/bin/tensorfold serve ${modelId}"
          + " --host 0.0.0.0 --port 8888"
          + " --name Qwen3.8-Flash-Next"
          + " --parallel 5 --context 262144 --kv-dtype int8"
          + " --mtp-drafts 6 --mtp-confidence 0.60"
          + " --temperature 1.0 --top-p 0.95 --top-k 20"
          + " --thinking --ple-on-ssd --vision";
        # The container ran with --ulimit memlock=-1 --ulimit stack=67108864.
        LimitMEMLOCK = "infinity";
        LimitSTACK = 67108864;
        # The first start downloads the ~106 GiB checkpoint (hours on a
        # LAN uplink) and JIT-compiles the kernels, so no start timeout:
        # the unit is in "activating" until ExecStartPre/ExecStart run.
        TimeoutStartSec = 0;
        TimeoutStopSec = 120;
        # SIGINT (not SIGTERM) so tensorfold can drain streams cleanly.
        KillSignal = "SIGINT";
      };
    };

  systemd.tmpfiles.rules = [
    "d /var/lib/qwen38-tensorfold 0755 qwen38 qwen38 - -"
    "d /var/lib/qwen38-tensorfold/kernels 0755 qwen38 qwen38 - -"
    "d /var/lib/vllm 0755 root root - -"
    "d /var/lib/vllm/huggingface 0755 qwen38 qwen38 - -"
  ];

  # Qwen3.8 is served from its HuggingFace repo ID through the native
  # TensorFold service above, with /var/lib/vllm/huggingface as its HF cache. The
  # declarative vllm-models downloader (written for the older vLLM
  # wrapper) stays off so it cannot pull a second copy of the weights.
  services.vllm-models.enable = false;

  services.dgx-dashboard = {
    enable = pkgs.lib.mkForce false;
    port = 11001;
  };

  # ─── mDNS (Avahi) ──────────────────────────────────────────────────
  services.avahi = {
    enable = true;
    publish = {
      enable = true;
      addresses = true;
      workstation = true;
    };
  };

  # ─── SSH ───────────────────────────────────────────────────────────
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
  };

  # ─── ZRAM swap ──────────────────────────────────────────────────────
  zramSwap.enable = true;

  # ─── System packages ───────────────────────────────────────────────
  environment.systemPackages = with pkgs; [
    curl
    git
    htop
    tmux
    tree
    vim
    wget
    fastfetch
    pciutils
    ethtool
    rdma-core
    fwupd
    # Regression tools for the TensorFold service (bench/needle/toolcheck/
    # visioncheck) — the container-vs-native performance gate runs through
    # these; see the deployment repo's README for the baseline table.
    tensorfold-tools
  ];

  # fwupd-refresh.service (fwupdmgr refresh) requires polkit auth and fails during
  # non-interactive activation (colmena deploy). Tolerate the failure so it doesn't
  # abort the deployment — manual `fwupdmgr refresh/update` still works.
  systemd.services.fwupd-refresh.serviceConfig.SuccessExitStatus = [
    0
    1
  ];

  # ─── Retired vLLM model cache cleanup ──────────────────────────────
  # Commit 422cbfd1 switched the DGX Spark vLLM service to serve Qwen3.6
  # directly from HuggingFace into /var/lib/vllm/huggingface. Prune the old
  # declarative /var/lib/vllm/models downloads once after deployment.
  systemd.services.vllm-prune-obsolete-models =
    let
      obsoleteModels = [
        "Qwen3.6-27B-AEON-NVFP4"
        "Qwen3.6-27B-DFlash-drafter"
        "Qwen3.6-35B-A3B-NVFP4"
        "Ornith-1.0-35B-NVFP4"
        "AEON-DFlash-Qwen3.6-35B-A3B"
      ];
      stamp = "/var/lib/vllm/.pruned-obsolete-models-422cbfd1";
      pruneScript = pkgs.writeShellScript "vllm-prune-obsolete-models" ''
        set -eu

        if [ -e ${stamp} ]; then
          exit 0
        fi

        ${pkgs.coreutils}/bin/mkdir -p /var/lib/vllm/models
        for model in ${toString obsoleteModels}; do
          path="/var/lib/vllm/models/$model"
          case "$path" in
            /var/lib/vllm/models/*) ;;
            *) echo "Refusing to remove unexpected path: $path" >&2; exit 1 ;;
          esac

          if [ -e "$path" ]; then
            echo "Removing obsolete vLLM model cache: $path"
            ${pkgs.coreutils}/bin/rm -rf --one-file-system -- "$path"
          fi
        done

        ${pkgs.coreutils}/bin/touch ${stamp}
      '';
    in
    {
      description = "Prune obsolete vLLM model downloads retired by 422cbfd1";
      after = [ "local-fs.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = pruneScript;
      };
    };

  # ─── HuggingFace token ─────────────────────────────────────────────
  # Secret file (secrets/hf-token.age) must contain:
  #   HF_TOKEN=hf_xxxxxxxxxxxx
  age.secrets.hf-token = {
    file = ../../secrets/hf-token.age;
    owner = "yuanw";
    group = "users";
  };

  # Source HF_TOKEN for user shells (systemd services use EnvironmentFile)
  environment.etc."profile.d/hf-token.sh".text = ''
    # agenix secret contains: HF_TOKEN=hf_xxx
    if [ -s ${config.age.secrets.hf-token.path} ]; then
      set -a; . ${config.age.secrets.hf-token.path}; set +a
    fi
  '';

  system.stateVersion = "25.11";
}
