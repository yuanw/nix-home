{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
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
  };
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
  nixpkgs.config.allowUnfree = true;

  # ─── Firewall ──────────────────────────────────────────────────────
  networking.firewall.allowedTCPPorts = [ 11000 ]; # DGX Dashboard

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
  ];

  # ─── Zsh ───────────────────────────────────────────────────────────
  #programs.zsh.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data were taken. Do NOT change this after
  # the initial install.
  system.stateVersion = "25.11";
}
