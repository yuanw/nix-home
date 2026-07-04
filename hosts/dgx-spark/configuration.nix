{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/comfyui.nix
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
  users.users.yuanw = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "video"
      "docker"
    ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMSvr2qkdnG03/pGLo3aCFTnwmvojKO6m/W74ckC1RPW me@yuanwang.ca"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFpYgmWwtRG7vlRbtWheYrtHl9E9qx84sdU+YlE8w+CZ me@yuanwang.ca"
    ];
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

  # ─── DGX Dashboard ─────────────────────────────────────────────────
  networking.firewall.allowedTCPPorts = [
    8000 # vLLM aeon (Qwen3.6-27B)
    8001 # vLLM gemma
    8002 # vLLM qwen35b
    8003 # vLLM ornith (AEON container, NVFP4 + DFlash)
    11000
    8188
  ];

  services.comfyui = {
    enable = false;
    host = "0.0.0.0";
    port = 8188;
    openFirewall = true;
  };

  # ─── vLLM Inference ─────────────────────────────────────────────────
  services.vllm.instances = {
    # Primary: Qwen3.6-27B NVFP4 + DFlash speculative decoding
    # Best single-stream throughput on DGX Spark (38-56 tok/s by category)
    aeon = {
      enable = false;
      autoStart = false; # Start manually after model downloads complete
      backend = "podman";
      containerImage = "ghcr.io/aeon-7/aeon-vllm-ultimate:2026-06-18-v0.23.0-dflashfix";
      model = "/var/lib/vllm/models/Qwen3.6-27B-AEON-NVFP4";
      servedModelName = "aeon";
      port = 8000;
      gpuMemoryUtilization = 0.78;
      maxModelLen = 24576;
      maxNumSeqs = 8;
      maxNumBatchedTokens = 8192;
      dtype = "auto";
      quantization = "compressed-tensors"; # NVFP4
      kvCacheDtype = null; # auto/BF16 (DFlash non-causal + fp8_e4m3 unsupported on sm_121a)
      enableChunkedPrefill = true;
      enablePrefixCaching = true;
      mambaBlockSize = 256; # Qwen3.6 hybrid GDN+attention
      toolCallParser = "qwen3_coder";
      speculative = {
        enable = true;
        model = "/var/lib/vllm/models/Qwen3.6-27B-DFlash-drafter";
        numSpeculativeTokens = 12;
      };
      extraArgs = [ "--trust-remote-code" ];
    };

    # Gemma-4-26B-A4B NVFP4 (fastest single-stream, 155 tok/s coding)
    gemma = {
      enable = false;
      backend = "podman";
      containerImage = "ghcr.io/aeon-7/aeon-vllm-ultimate:2026-06-18-v0.23.0-dflashfix";
      model = "/var/lib/vllm/models/Gemma-4-26B-A4B-NVFP4";
      servedModelName = "gemma";
      port = 8001;
      gpuMemoryUtilization = 0.78;
      maxModelLen = 32768;
      maxNumSeqs = 8;
      quantization = "compressed-tensors";
      kvCacheDtype = "fp8_e4m3";
      enableChunkedPrefill = true;
      enablePrefixCaching = true;
      speculative = {
        enable = true;
        model = "/var/lib/vllm/models/Gemma-4-26B-A4B-DFlash-drafter";
        numSpeculativeTokens = 12;
      };
      extraArgs = [ "--trust-remote-code" ];
    };

    # Qwen3.6-35B-A3B NVFP4 (largest MoE model that fits)
    # Uses official NVIDIA recommended config from model card:
    # https://huggingface.co/nvidia/Qwen3.6-35B-A3B-NVFP4#usage
    qwen35b = {
      enable = true;
      autoStart = false; # Start manually after model downloads complete
      backend = "podman";
      # spark-vllm-docker build (vllm-node) — custom vLLM built for DGX Spark
      # with DeepGEMM, FlashInfer, and sm_121a support. Prebuilt wheels from:
      #   https://github.com/eugr/spark-vllm-docker/releases
      # Benchmark: 109 tok/s on spark-arena.com/benchmark/sub1782724431960
      containerImage = "localhost/vllm-node:latest";
      model = "/var/lib/vllm/models/Qwen3.6-35B-A3B-NVFP4";
      servedModelName = "qwen35b";
      port = 8002;
      # spark-arena-tested DGX Spark params (from sparkrun recipe)
      gpuMemoryUtilization = 0.65;
      maxModelLen = 262144;
      maxNumSeqs = 4;
      maxNumBatchedTokens = 32768;
      dtype = "auto";
      quantization = "modelopt"; # NVFP4 via Model Optimizer
      kvCacheDtype = "fp8";
      enableChunkedPrefill = true;
      enablePrefixCaching = true;
      # No speculative decoding — MTP requires the built-in MTP heads but
      # the spark-vllm-docker image doesn't include DFlash.
      speculative = {
        enable = false;
      };
      reasoningParser = "qwen3";
      toolCallParser = "qwen3_xml";
      extraEnv = {
        VLLM_MARLIN_USE_ATOMIC_ADD = "1";
      };
      extraArgs = [
        "--trust-remote-code"
        "--attention-backend"
        "flashinfer"
        "--moe-backend"
        "marlin"
        "--async-scheduling"
        "--load-format"
        "fastsafetensors"
        "--enable-auto-tool-choice"
      ];
    };

    # Ornith-1.0-35B AEON Ultimate Uncensored — NVFP4 + DFlash via the AEON
    # container (ghcr.io/aeon-7/aeon-vllm-ultimate), which ships the
    # qwen3_5_moe arch + DFlash pre-compiled for GB10 / sm_121a (the runtime
    # gap that the native path still hits, see native-plan Verification).
    # Validated DGX Spark envelope from QUICKSTART_DGX_SPARK.md § 3/§ 4.
    ornith = {
      enable = true; # Phase 1 verified 2026-06-28 (CDI + AEON image OK)
      autoStart = false; # one-shot start after model downloads complete
      backend = "podman";
      containerImage = "ghcr.io/aeon-7/aeon-vllm-ultimate:2026-06-18-v0.23.0-dflashfix";
      model = "/var/lib/vllm/models/Ornith-1.0-35B-NVFP4";
      servedModelName = "ornith";
      port = 8003; # host port == container port (podman --network host)
      # DFlash stability margin on unified memory (peak 80/121 GB).
      gpuMemoryUtilization = 0.6;
      maxModelLen = 262144; # full 256K fits thanks to NVFP4 (~23.7 GB)
      maxNumSeqs = 16; # HARD CAP w/ DFlash on Spark (assertion enforces)
      maxNumBatchedTokens = 16384;
      dtype = "auto";
      quantization = "compressed-tensors"; # NVFP4 = nvfp4-pack-quantized
      kvCacheDtype = null; # BF16 KV (vision tower) — do NOT pass fp8
      mambaCacheDtype = "float32"; # GatedDeltaNet (SSM) state precision
      reasoningParser = "qwen3";
      toolCallParser = "qwen3_coder"; # QUICKSTART value (serve_ornith.sh uses qwen3_xml)
      enableChunkedPrefill = true;
      enablePrefixCaching = true;
      mambaBlockSize = 256;
      speculative = {
        enable = true;
        # AEON all-full-attention drafter: no SWA → no kvfix patch (issue #1),
        # and higher acceptance than z-lab (3.71 vs 3.35 tok/step).
        model = "/var/lib/vllm/models/AEON-DFlash-Qwen3.6-35B-A3B";
        numSpeculativeTokens = 6; # QUICKSTART optimum; n>6 wastes on low-accept pos
      };
      extraEnv = {
        TORCH_CUDA_ARCH_LIST = "12.1a";
        ENABLE_NVFP4_SM100 = "0";
        VLLM_USE_FLASHINFER_SAMPLER = "1";
        NVIDIA_FORWARD_COMPAT = "1";
        NVIDIA_DRIVER_CAPABILITIES = "all";
      };
      extraArgs = [ "--trust-remote-code" ];
    };
  };

  # Declarative model downloads (oneshot services, idempotent)
  services.vllm-models = {
    enable = true;
    cacheDir = "/var/lib/vllm/models";
    models = {
      # Qwen3.6-27B body (NVFP4 compressed-tensors, ~26 GB)
      "Qwen3.6-27B-AEON-NVFP4" = {
        repo = "AEON-7/Qwen3.6-27B-AEON-Ultimate-Uncensored-NVFP4";
      };
      # DFlash drafter for Qwen3.6-27B (z-lab 5-layer, ~3.3 GB)
      "Qwen3.6-27B-DFlash-drafter" = {
        repo = "z-lab/Qwen3.6-27B-DFlash";
      };

      # ── NVIDIA Qwen3.6-35B-A3B NVFP4 (official release) ──
      # Official NVIDIA release of Qwen3.6-35B in NVFP4 compressed-tensors format.
      # Base model (no drafter bundled); shares the AEON DFlash drafter above.
      "Qwen3.6-35B-A3B-NVFP4" = {
        repo = "nvidia/Qwen3.6-35B-A3B-NVFP4";
      };

      # ── Ornith-1.0-35B AEON Ultimate Uncensored (container path) ──
      # NVFP4 model (~23.7 GB, Blackwell-only) + AEON all-full-attention
      # DFlash drafter (no SWA, so no kvfix patch needed).
      "Ornith-1.0-35B-NVFP4" = {
        repo = "AEON-7/Ornith-1.0-35B-AEON-Ultimate-Uncensored-NVFP4";
      };
      "AEON-DFlash-Qwen3.6-35B-A3B" = {
        repo = "AEON-7/AEON-DFlash-Qwen3.6-35B-A3B";
      };
    };
  };

  services.dgx-dashboard = {
    enable = true;
    port = 11001;
  };

  systemd.sockets.dgx-dashboard-lan = {
    description = "DGX Dashboard LAN socket";
    wantedBy = [ "sockets.target" ];
    listenStreams = [ "11000" ];
  };
  systemd.services.dgx-dashboard-lan = {
    description = "DGX Dashboard LAN proxy";
    requires = [ "dgx-dashboard-lan.socket" ];
    after = [
      "dgx-dashboard-lan.socket"
      "dgx-dashboard.service"
    ];
    serviceConfig = {
      ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd 127.0.0.1:11001";
      PrivateTmp = true;
    };
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
    ds4
  ];

  # fwupd-refresh.service (fwupdmgr refresh) requires polkit auth and fails during
  # non-interactive activation (colmena deploy). Tolerate the failure so it doesn't
  # abort the deployment — manual `fwupdmgr refresh/update` still works.
  systemd.services.fwupd-refresh.serviceConfig.SuccessExitStatus = [
    0
    1
  ];

  # ─── vllm-node image build ──────────────────────────────────────
  # Builds the spark-vllm-docker Docker image from pinned inputs
  # when the package changes (new source commit or wheel hashes).
  systemd.services.vllm-node-build = {
    description = "Build vllm-node Docker image from pinned inputs";
    after = [ "network.target" ];
    wants = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    path = with pkgs; [ podman ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.vllm-node}/bin/build-vllm-node";
      TimeoutStartSec = 1800; # 30 min for first build
    };
  };

  # The qwen35b instance requires the vllm-node image to be built first
  systemd.services.vllm-qwen35b = {
    requires = [ "vllm-node-build.service" ];
    after = [ "vllm-node-build.service" ];
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
