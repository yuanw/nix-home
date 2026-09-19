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
    8000 # vLLM qwen38 (MiaAI-Lab Qwen3.8 Flash Next single-Spark recipe)
    11000
    8188
  ];

  # ─── vLLM Inference ─────────────────────────────────────────────────
  # Qwen3.8-Flash-Next does not fit the generic services.vllm.instances
  # wrapper cleanly: the single-DGX-Spark recipe relies on its launcher to
  # prepare PLE offload patches, build/use the packed PLE mmap table, mount a
  # reduced MTP draft vocabulary, and run the watchdog. Keep the service
  # manual; first populate the HF cache with:
  #   cd /var/lib/qwen38-flash-next/repo && ./download.sh
  systemd.services.vllm-qwen38 =
    let
      repoDir = "/var/lib/qwen38-flash-next/repo";
      prepare = pkgs.writeShellScript "prepare-qwen38-flash-next" ''
        set -eu

        install -d -m 0755 /var/lib/qwen38-flash-next
        install -d -o yuanw -g users -m 0755 /var/lib/vllm/huggingface

        if [ ! -d ${repoDir}/.git ]; then
          rm -rf ${repoDir}
          git clone --depth 1 https://github.com/MiaAI-Lab/Qwen3.8-Flash-Next-Single-DGX-Spark ${repoDir}
        else
          git -C ${repoDir} fetch --depth 1 origin main
          git -C ${repoDir} reset --hard origin/main
        fi

        cat > ${repoDir}/.env <<'EOF'
        IMAGE="vllm/vllm-openai:qwen38-flash-next"
        SERVED_MODEL_NAME="qwen3.8-flash-next"
        TP1_CONTAINER_NAME="vllm-qwen38"

        # Keep the old local API port while switching the served model.
        PORT=8000
        BIND=0.0.0.0

        # MiaAI-Lab measured default profile for one DGX Spark.
        YARN=0
        MAX_MODEL_LEN=262144
        YARN_MAX_MODEL_LEN=524288
        MTP_NUM_SPECULATIVE_TOKENS=3
        KV_TARGET_GIB=20
        HOST_RESERVE_GIB=26
        KV_CACHE_DTYPE=fp8
        MAMBA_SSM_CACHE_DTYPE=bfloat16
        MAX_NUM_SEQS=4
        MAX_NUM_BATCHED_TOKENS=2048
        CUDAGRAPH_CAPTURE_SIZES=auto
        MTP_DRAFT_VOCAB=files/draft_vocab_en_code_47k.txt
        EXTRA_DOCKER_ARGS="-e VLLM_USE_V2_MODEL_RUNNER=1"

        PLE_OFFLOAD=true
        REQUIRE_IDLE_GPU=true
        READY_TIMEOUT_S=1800
        EOF
        sed -i 's/^        //' ${repoDir}/.env
        chmod 0600 ${repoDir}/.env
      '';
    in
    {
      description = "vLLM Qwen3.8 Flash Next single-DGX-Spark server";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      conflicts = [ "vllm-qwen36.service" ];
      wantedBy = [ ]; # start manually: systemctl start vllm-qwen38

      path = with pkgs; [
        bash
        coreutils
        curl
        docker-client
        gawk
        git
        gnugrep
        gnused
        procps
        util-linux
      ];

      environment = {
        HF_HOME = "/var/lib/vllm/huggingface";
        HOME = "/var/lib/qwen38-flash-next";
      };

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        WorkingDirectory = "/var/lib/qwen38-flash-next";
        EnvironmentFile = [ config.age.secrets.hf-token.path ];
        ExecStartPre = prepare;
        ExecStart = "${pkgs.bash}/bin/bash ${repoDir}/start.sh";
        ExecStop = "${pkgs.bash}/bin/bash ${repoDir}/stop.sh";
        TimeoutStartSec = 2400;
        TimeoutStopSec = 120;
      };
    };

  systemd.tmpfiles.rules = [
    "d /var/lib/qwen38-flash-next 0755 root root - -"
    "d /var/lib/vllm 0755 root root - -"
    "d /var/lib/vllm/huggingface 0755 yuanw users - -"
  ];

  # Qwen3.8 is served from its HuggingFace repo ID through the upstream
  # launcher, using /var/lib/vllm/huggingface as the shared HF cache.
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
