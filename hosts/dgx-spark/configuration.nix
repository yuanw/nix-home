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
      enable = true;
      autoStart = false; # Start manually after model downloads complete
      model = "/var/lib/vllm/models/Qwen3.6-27B-AEON-NVFP4";
      servedModelName = "aeon";
      port = 8000;
      gpuMemoryUtilization = 0.78;
      maxModelLen = 24576;
      maxNumSeqs = 8;
      maxNumBatchedTokens = 8192;
      dtype = "auto";
      quantization = "compressed-tensors"; # NVFP4
      kvCacheDtype = "fp8_e4m3";
      enableChunkedPrefill = true;
      enablePrefixCaching = true;
      mambaBlockSize = 256; # Qwen3.6 hybrid GDN+attention
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
    qwen35b = {
      enable = false;
      model = "/var/lib/vllm/models/Qwen3.6-35B-A3B-NVFP4";
      servedModelName = "qwen35b";
      port = 8002;
      gpuMemoryUtilization = 0.76; # More conservative for larger model
      maxModelLen = 24576;
      maxNumSeqs = 8;
      quantization = "compressed-tensors";
      kvCacheDtype = "fp8_e4m3";
      enableChunkedPrefill = true;
      enablePrefixCaching = true;
      mambaBlockSize = 256;
      speculative = {
        enable = true;
        model = "/var/lib/vllm/models/Qwen3.6-35B-A3B-DFlash-drafter";
        numSpeculativeTokens = 12;
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
