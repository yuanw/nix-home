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

    qwen38 = {
      isSystemUser = true;
      group = "qwen38";
      home = "/var/lib/qwen38-flash-next";
      createHome = false;
      description = "Qwen3.8 Flash Next vLLM state owner";
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

  # ─── DGX Dashboard ─────────────────────────────────────────────────
  networking.firewall.allowedTCPPorts = [
    8000 # vLLM qwen38 (MiaAI-Lab Qwen3.8 Flash Next single-Spark recipe)
    8888 # TensorFold qwen38 (Qwen3.8 Flash Next TensorFold recipe)
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
  #
  # vLLM variant disabled while trying out the TensorFold variant below
  # (they serve the same model on port 8000/8888 and conflict).
  /*
    systemd.services.vllm-qwen38 =
      let
        repoDir = "/var/lib/qwen38-flash-next/repo";
        qwen38User = "qwen38";
        qwen38Group = "qwen38";
        qwen38SingleSparkRev = "b8439110eec0230facbe4ddf0dffe01b8f769be0";
        qwen38SingleSpark = pkgs.fetchFromGitHub {
          owner = "yuanw";
          repo = "Qwen3.8-Flash-Next-Single-DGX-Spark";
          rev = qwen38SingleSparkRev;
          hash = "sha256-sMmFTesW8+TR81CaGQVT77XQC2AqkBxBJQV6AyDxUIc=";
        };
        prepare = pkgs.writeShellScript "prepare-qwen38-flash-next" ''
          set -eu

          if [ ! -e ${repoDir}/.nix-source-rev ] || [ "$(cat ${repoDir}/.nix-source-rev)" != "${qwen38SingleSparkRev}" ]; then
            rm -rf ${repoDir}
            install -d -o ${qwen38User} -g ${qwen38Group} -m 0755 ${repoDir}
            cp -a ${qwen38SingleSpark}/. ${repoDir}/
            chmod -R u+w ${repoDir}
            chown -R ${qwen38User}:${qwen38Group} ${repoDir}
            printf '%s\n' '${qwen38SingleSparkRev}' > ${repoDir}/.nix-source-rev
            chown ${qwen38User}:${qwen38Group} ${repoDir}/.nix-source-rev
          fi

          cat > ${repoDir}/.env <<'EOF'
          # Managed by Nix. Runtime configuration is supplied by
          # systemd.services.vllm-qwen38.environment.
          EOF
          sed -i 's/^        //' ${repoDir}/.env
          chmod 0600 ${repoDir}/.env
          chown ${qwen38User}:${qwen38Group} ${repoDir}/.env
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
          # NOTE: broken for GPU: the real Docker CLI's DeviceRequests for
          # `--gpus all` are silently dropped by the podman compat socket, so
          # containers get no GPU. Use a docker->podman symlink instead (see
          # vllm-qwen38-tensorfold) if this service is ever re-enabled.
          docker-client
          gawk
          git
          gnugrep
          gnused
          inetutils
          procps
          python3
          util-linux
        ];

        environment = {
          HF_HOME = "/var/lib/vllm/huggingface";
          HOME = "/var/lib/qwen38-flash-next";

          IMAGE = "docker.io/vllm/vllm-openai:qwen38-flash-next";
          SERVED_MODEL_NAME = "qwen3.8-flash-next";
          TP1_CONTAINER_NAME = "vllm-qwen38";

          # Keep the old local API port while switching the served model.
          PORT = "8000";
          BIND = "0.0.0.0";

          # MiaAI-Lab measured default profile for one DGX Spark.
          YARN = "0";
          MAX_MODEL_LEN = "262144";
          YARN_MAX_MODEL_LEN = "524288";
          MTP_NUM_SPECULATIVE_TOKENS = "3";
          KV_TARGET_GIB = "20";
          HOST_RESERVE_GIB = "26";
          KV_CACHE_DTYPE = "fp8";
          MAMBA_SSM_CACHE_DTYPE = "bfloat16";
          MAX_NUM_SEQS = "4";
          MAX_NUM_BATCHED_TOKENS = "2048";
          CUDAGRAPH_CAPTURE_SIZES = "auto";
          MTP_DRAFT_VOCAB = "files/draft_vocab_en_code_47k.txt";
          EXTRA_DOCKER_ARGS = "-e VLLM_USE_V2_MODEL_RUNNER=1";

          PLE_OFFLOAD = "true";
          REQUIRE_IDLE_GPU = "true";
          READY_TIMEOUT_S = "1800";
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
  */

  # ─── TensorFold Inference ─────────────────────────────────────────────
  # Same model, different engine: Qwen3.8-Flash-Next-Single-DGX-Spark-
  # TensorFold serves Vontra/Qwen3.8-Flash-Next-MLX-4bit-MTP through
  # TensorFold v0.3.6.3 in a docker container (5 streams x 262,144 tokens,
  # int8 KV, vision tower). Like the vLLM variant, its launcher
  # (start.sh + scripts/prepare.sh) does not fit the generic
  # services.vllm.instances wrapper: prepare.sh pulls/builds the patched
  # image and downloads the ~106 GiB checkpoint on the first start, so the
  # service is manual and has no start timeout. First start:
  #   systemctl start vllm-qwen38-tensorfold   # then: journalctl -fu vllm-qwen38-tensorfold
  systemd.services.vllm-qwen38-tensorfold =
    let
      repoDir = "/var/lib/qwen38-tensorfold/repo";
      qwen38User = "qwen38";
      qwen38Group = "qwen38";
      tensorFoldRev = "a3aa89835022c55ca8e55008c37785954834e04f";
      tensorFoldSrc = pkgs.fetchFromGitHub {
        owner = "yuanw";
        repo = "Qwen3.8-Flash-Next-Single-DGX-Spark-TensorFold";
        rev = tensorFoldRev;
        hash = "sha256-upiScG4RoX6Ff4v/nSJwQEOBiXv04Z409KMUAXopgs0=";
      };
      # The box has no Docker daemon: `docker` is podman in dockerCompat mode.
      # The real Docker CLI (pkgs.docker-client) sends HostConfig.DeviceRequests
      # for `--gpus all`, which the podman compat API silently drops, so the
      # container starts with Devices=[] and torch finds no NVIDIA driver.
      # A `docker` symlink to podman makes podman translate --gpus to CDI
      # devices itself (same as /run/current-system/sw/bin/docker).
      dockerShim = pkgs.runCommand "docker-podman-shim" { } ''
        mkdir -p $out/bin
        ln -s ${pkgs.podman}/bin/podman $out/bin/docker
      '';
      prepare = pkgs.writeShellScript "prepare-qwen38-tensorfold" ''
        set -eu

        if [ ! -e ${repoDir}/.nix-source-rev ] || [ "$(cat ${repoDir}/.nix-source-rev)" != "${tensorFoldRev}" ]; then
          rm -rf ${repoDir}
          install -d -o ${qwen38User} -g ${qwen38Group} -m 0755 ${repoDir}
          cp -a ${tensorFoldSrc}/. ${repoDir}/
          chmod -R u+w ${repoDir}
          chown -R ${qwen38User}:${qwen38Group} ${repoDir}
          printf '%s\n' '${tensorFoldRev}' > ${repoDir}/.nix-source-rev
          chown ${qwen38User}:${qwen38Group} ${repoDir}/.nix-source-rev
        fi

        cat > ${repoDir}/.env <<'EOF'
        # Managed by Nix. Runtime configuration is supplied by
        # systemd.services.vllm-qwen38-tensorfold.environment.
        EOF
        sed -i 's/^        //' ${repoDir}/.env
        chmod 0600 ${repoDir}/.env
        chown ${qwen38User}:${qwen38Group} ${repoDir}/.env
      '';
    in
    {
      description = "TensorFold Qwen3.8 Flash Next single-DGX-Spark server";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      conflicts = [ "vllm-qwen38.service" ];
      wantedBy = [ ]; # start manually: systemctl start vllm-qwen38-tensorfold

      path = with pkgs; [
        bash
        coreutils
        curl
        dockerShim # NOT pkgs.docker-client: see comment above
        findutils
        gawk
        git
        gnugrep
        gnused
        hostname-debian # start.sh uses `hostname -I` (net-tools syntax); inetutils' hostname exits 64 on it
        iproute2
        procps
        python3
        util-linux
      ];

      environment = {
        # start.sh derives HF_CACHE and KERNEL_CACHE from HOME (with HF_CACHE
        # overridden below); .env in the repo dir is written by prepare.
        HOME = "/var/lib/qwen38-tensorfold";

        # Share the HF cache with the vLLM variant.
        HF_CACHE = "/var/lib/vllm/huggingface";

        # scripts/config.sh defaults (all overridable here).
        CONTAINER_NAME = "qwen38-flash-next-tf";
        SERVED_NAME = "Qwen3.8-Flash-Next";
        HOST = "0.0.0.0";
        PORT = "8888";
        WAIT_TIMEOUT = "1800";
      };

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        WorkingDirectory = "/var/lib/qwen38-tensorfold";
        EnvironmentFile = [ config.age.secrets.hf-token.path ];
        ExecStartPre = prepare;
        ExecStart = "${pkgs.bash}/bin/bash ${repoDir}/start.sh";
        ExecStop = "${pkgs.bash}/bin/bash ${repoDir}/stop.sh";
        # The first start pulls the ~11 GB image and downloads the ~106 GiB
        # checkpoint: no start timeout.
        TimeoutStartSec = 0;
        TimeoutStopSec = 120;
      };
    };

  systemd.tmpfiles.rules = [
    "d /var/lib/qwen38-flash-next 0755 qwen38 qwen38 - -"
    "d /var/lib/qwen38-tensorfold 0755 qwen38 qwen38 - -"
    "d /var/lib/vllm 0755 root root - -"
    "d /var/lib/vllm/huggingface 0755 qwen38 qwen38 - -"
  ];

  # Qwen3.8 is served from its HuggingFace repo ID through the upstream
  # launcher, using /var/lib/vllm/huggingface as the shared HF cache.
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
