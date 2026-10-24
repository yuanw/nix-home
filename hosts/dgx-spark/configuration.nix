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

  # ─── GPU clock cap (thermal protection) ─────────────────────────────
  # The GB10 burns 140W inside 1.13 L with firmware cooling that exposes
  # neither a fan curve nor a power limit (`nvidia-smi -pl` is N/A). Under
  # sustained inference its SM clock sits at the 3003 MHz boost limit, temps
  # park in the 85 °C danger zone, and the box eventually thermal-shuts-down
  # on a concurrent-prefill power spike. Locking the clock to 2200 MHz drops
  # it ~20 °C for no measurable throughput loss, because MoE inference here
  # is HBM-bandwidth-bound, not clock-bound.
  # The lock is not persistent across reboots, so reapply it every boot, and
  # ahead of any unit that touches the GPU. Add other GPU services to
  # `before` when they appear (red-snow, tensorfold-zig).
  # https://www.wildpines.ai/blog/your-dgx-spark-is-cooking-itself/
  systemd.services.gpu-clock-cap = {
    description = "Cap GPU SM clocks to 2200 MHz for thermal protection";
    after = [ "nvidia-persistenced.service" ];
    wantedBy = [ "multi-user.target" ];
    before = [ "tensorfold.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      # Belt and suspenders: persistenced should have done this already.
      ExecStartPre = "${config.system.path}/bin/nvidia-smi -pm 1";
      # The second field is the *cap* (3003 MHz is the default boost); the
      # first only keeps the idle floor, so low utilisation still downclocks.
      ExecStart = "${config.system.path}/bin/nvidia-smi -lgc 300,2200";
    };
  };

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
        # silver's nix daemon logs in as yuanw to build aarch64-linux closures
        # here (docs/harmonia-build-hub.org 3.1); the private half is
        # secrets/nix-builder-key.age on silver.  maxJobs 1 in buildMachines
        # keeps this box's inference workload from being starved.
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeGertBlb+dywvvhHD4vpxH3D7B85q7jkzWSKatVIbJ silver-build-hub"
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
    # The build hub dispatches aarch64-linux drvs here and colmena builds this
    # host's closure on itself, while an inference model is resident (tensorfold
    # measured 86 G RSS of 121 G on 2026-10-24).  buildMachines.maxJobs = 1 on
    # silver only limits how many drvs it hands over at once; these two are what
    # cap what a *single* drv does to the box.  Without `cores` the first CUDA
    # drv spawned 20 nvcc + 20 ptxas (load 38) and ate the headroom the model
    # needs, which is how a cache warm turns into an OOM.
    max-jobs = 1;
    cores = 8;
    trusted-users = [
      "root"
      "yuanw"
    ];
    substituters = [
      # Our own aarch64-linux CUDA outputs, pushed from silver by
      # `just cache-push` (modules/services/cache/push.nix).  The CUDA stack here
      # has no public cache entry — magma, and the libnvshmem patched for CUDA
      # 13.2 in cuda-fixes.nix — so a pin bump recompiles thousands of .cu files
      # on this box unless they come from here.  A deploy after `nix flake
      # update` measured 3492 magma build steps alone.
      "https://dgx-spark-nixos.cachix.org"
      "https://nix-community.cachix.org"
      "https://cuda-maintainers.cachix.org"
      "https://cache.nixos-cuda.org"
      "https://graham33.cachix.org"
      "https://ai.cachix.org"
    ];
    trusted-public-keys = [
      "dgx-spark-nixos.cachix.org-1:Ps3d91fOvYEtxhTS59k2S1Y3P7ld6ahhx7nHKKF3ojc="
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

  # ssh-ng builds and colmena's build-on-target runs are yuanw login sessions, so
  # they live in this slice -- and user slices are named by UID, not username
  # (`user-1000.slice`; a `user-yuanw.slice` unit is simply never instantiated,
  # verified against /proc/<ninja>/cgroup).  Not nix-daemon.service's cgroup
  # either, which is why a MemoryMax on that unit does nothing for remote builds.
  #
  # Weight bounds contention, MemoryMax bounds the disaster: a torch build put
  # four `cicc` at 4 G each on the box with 17 G spare and the 86 G model
  # resident.  With the cap it is the build that gets OOM-killed, not tensorfold
  # (which is what died on 2026-10-14).
  systemd.units."user-${toString config.users.users.yuanw.uid}.slice".text = ''
    [Slice]
    CPUWeight=20
    MemoryHigh=32G
    MemoryMax=40G
  '';
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
  # The Python TensorFold (v0.6.1, Vontra MLX-4bit) is the *disabled*
  # fallback: the Zig engine below won on simplicity at equal quality
  # (Spark-Bench 81.7 vs 81.4). To revert, set enable = true here and
  # `services.tensorfold-zig.enable = false`; both serve :8888, so the
  # provider needs no change. See docs/tensorfold-zig-plan.org.
  services.tensorfold = {
    enable = false;
    openFirewall = true;
    environmentFile = config.age.secrets.hf-token.path;
  };

  # ─── TensorFold Zig (tensorfold-native) ─────────────────────────
  # modules/tensorfold-zig.nix + packages/tensorfold-zig: upstream
  # TensorFold's Zig engine (branch zig-flashnext) packaged natively,
  # serving Qwen3.8 Flash Next from NVIDIA's FP4 QAD checkpoint
  # (nvidia/Qwen3.8-Flash-Next-FP4-Local, ~96 GiB: the unit downloads it on
  # first start) on :8888. The CUDA fatbins + Triton AOT cubins are baked in
  # (no first-start JIT). The former INT4-AutoRound checkpoint (~122 GiB) is
  # still in the HF cache; prune it by hand if the disk needs the space.
  # Plan: docs/tensorfold-zig-plan.org
  services.tensorfold-zig = {
    enable = true;
    openFirewall = true;
    environmentFile = config.age.secrets.hf-token.path;
    # Recipe patch 0010 (issue #33): keep the ATS-locked system-memory caches and
    # native VMM rings, instead of the keeper pool, so steady load does not emit
    # NV_ERR_NO_MEMORY. ringCache stays available but off.
    systemMemoryCache = true;
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
