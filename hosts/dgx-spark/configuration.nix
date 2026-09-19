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
  users.users.yuanw = {
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
    8000 # vLLM qwen36 (local.ai original Qwen3.6 recipe)
    11000
    8188
  ];

  # ─── vLLM Inference ─────────────────────────────────────────────────
  services.vllm.instances = {
    # Qwen3.6-35B-A3B original/full-weight local.ai recipe.
    # Uses the official vLLM OpenAI image rather than nixpkgs#vllm because
    # nixpkgs currently marks CUDA vLLM broken and aarch64-linux unsupported.
    # Start manually with: systemctl start vllm-qwen36
    qwen36 = {
      enable = true;
      autoStart = false;
      backend = "podman";
      containerImage = "docker.io/vllm/vllm-openai:v0.23.0";
      model = "Qwen/Qwen3.6-35B-A3B";
      servedModelName = "Qwen/Qwen3.6-35B-A3B";
      port = 8000;
      gpuMemoryUtilization = 0.88;
      maxModelLen = 262144;
      maxNumSeqs = 64;
      maxNumBatchedTokens = 8192;
      dtype = "auto";
      kvCacheDtype = null;
      enableChunkedPrefill = true;
      enablePrefixCaching = true;
      reasoningParser = "qwen3";
      toolCallParser = "qwen3_xml";
      extraArgs = [
        "--tensor-parallel-size"
        "1"
        "--pipeline-parallel-size"
        "1"
        "--trust-remote-code"
        "--enable-prompt-tokens-details"
        "--enable-force-include-usage"
        "--enable-request-id-headers"
        "--enable-log-requests"
        "--block-size"
        "1024"
        "--kv-cache-memory-bytes"
        "32212254720"
        "--async-scheduling"
        "--language-model-only"
        "--mamba-cache-mode"
        "align"
        "--speculative-config"
        ''{"method":"mtp","num_speculative_tokens":4,"moe_backend":"triton","rejection_sample_method":"standard"}''
      ];
    };
  };

  # Qwen3.6 is served from its HuggingFace repo ID directly inside the
  # vLLM container, using /var/lib/vllm/huggingface as the shared HF cache.
  services.vllm-models.enable = false;

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
