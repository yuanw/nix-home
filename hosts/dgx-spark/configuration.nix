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
  # (TensorFold's API port 8888 is opened by services.tensorfold.openFirewall)

  # ─── TensorFold Inference (native Nix, no container) ───────────────
  # Defined in modules/tensorfold.nix: packages/tensorfold (upstream +
  # the deployment repo's single site-packages patch) on the CUDA-
  # enabled nixpkgs torch, JIT kernels compiled on first start into
  # stateDir/kernels. Serve args and TENSORFOLD_* env mirror the
  # deployment recipe's defaults (scripts/config.sh @ 4c0dea8).
  # First start (checkpoint download if missing + JIT kernels):
  #   systemctl start tensorfold   # then: journalctl -fu tensorfold
  # Regression tools live in environment.systemPackages (bench/needle/
  # toolcheck/visioncheck); the container baseline table is in the
  # deployment repo's README.
  services.tensorfold = {
    enable = true;
    openFirewall = true;
    environmentFile = config.age.secrets.hf-token.path;
  };

  # ─── TensorFold Zig (tensorfold-native) ─────────────────────────
  # modules/tensorfold-zig.nix + packages/tensorfold-zig: upstream
  # TensorFold's Zig engine (branch zig-flashnext) packaged natively,
  # serving Qwen3.8 Flash Next (INT4-AutoRound) on :8890. The CUDA
  # fatbins + Triton AOT cubins are baked in (no first-start JIT).
  # Plan: docs/tensorfold-zig-plan.org
  services.tensorfold-zig = {
    enable = true;
    openFirewall = true;
    environmentFile = config.age.secrets.hf-token.path;
  };

  # (the tensorfold state/kernels/HF-cache dir rules live in
  # modules/tensorfold.nix; only the shared /var/lib/vllm parent stays)
  systemd.tmpfiles.rules = [
    "d /var/lib/vllm 0755 root root - -"
  ];

  # ─── RED-SNOW-5.3-Flash 2.49bpw (native exllamav3) ──────────────
  # modules/red-snow.nix: packages/red-snow (vcruz305/exllamav3 @
  # 7c1636f precompiled for sm_121) + the deployment repo's stdlib
  # /v1 server (Weschera/RED-SNOW-5.3-Flash-2.49bpw-1x-DGX-Spark @
  # b96fac2). Serves on :8899 next to TensorFold's :8888 — but one
  # model at a time on 128 GB unified memory, so
  # `systemctl stop tensorfold` first. The unit runs as root because
  # serve.sh's page-cache drops (GB10 undercounts file cache as free
  # unified memory) need it.
  # First start (checkpoint download if missing, then a 4-6 min load):
  #   systemctl start red-snow   # then: journalctl -fu red-snow
  # Smoke test (chat speed + tool calls): the repo's test.sh is just
  # curl against :8899 — any client works. Remaining rollout phases
  # (build/apply, first start, acceptance): docs/redsnow-native-plan.org
  services.red-snow = {
    enable = true;
    openFirewall = true;
    environmentFile = config.age.secrets.hf-token.path;
  };

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
