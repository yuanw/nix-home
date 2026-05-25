{ pkgs, inputs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    # ComfyUI NixOS module from nixified-ai
    # ComfyUI module needs CUDA 13.2 overlay too (matches what we set in nixpkgs.overlays)
    (inputs.nixified-ai.nixosModules.comfyui {
      overlays = [
        (_final: prev: { cudaPackages = prev.cudaPackages_13_2; })
        inputs.nixified-ai.overlays.comfyui
        inputs.nixified-ai.overlays.models
        inputs.nixified-ai.overlays.fetchers
      ];
    })
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
  # networking.networkmanager.enable = true;  # uncomment if wifi is needed later

  # ─── Time zone / locale ─────────────────────────────────────────────
  time.timeZone = "America/Regina";
  i18n.defaultLocale = "en_US.UTF-8";

  # ─── User accounts ──────────────────────────────────────────────────
  users.users.yuanw = {
    isNormalUser = true;
    #shell = pkgs.zsh;
    # Temporary password for first-boot console access.
    # Change it after login with: passwd yuanw
    extraGroups = [
      "wheel"
      "video" # GPU access
      "docker" # Podman/docker compat
    ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMSvr2qkdnG03/pGLo3aCFTnwmvojKO6m/W74ckC1RPW me@yuanwang.ca"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFpYgmWwtRG7vlRbtWheYrtHl9E9qx84sdU+YlE8w+CZ me@yuanwang.ca"
    ];
  };

  # ─── Sudo ────────────────────────────────────────────────────────
  # Allow wheel group to sudo without password (headless rebuilds)
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
    # CUDA binary cache (speeds up torch/flash-attn/comfyui builds)
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

  # ─── DS4 Server ─────────────────────────────────────────────────────
  services.ds4.enable = false;

  # ─── ComfyUI ───────────────────────────────────────────────────────
  services.comfyui = {
    enable = true;
    acceleration = "cuda";
    host = "0.0.0.0";
    port = 8188;
    openFirewall = true;
    # Uncomment to download SD 1.5 model on first build:
    # models = [ inputs.nixified-ai.legacyPackages.${pkgs.system}.comfyuiModels.sd15-fp16 ];
  };

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
  # Move dashboard to internal port 11001, then proxy external 11000
  networking.firewall.allowedTCPPorts = [ 11000 ]; # DGX Dashboard LAN proxy
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
  # Publish hostname so clients can reach dgx-spark.local
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

  # ─── Zsh ───────────────────────────────────────────────────────────
  #programs.zsh.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data were taken. Do NOT change this after
  # the initial install.
  system.stateVersion = "25.11";
}
