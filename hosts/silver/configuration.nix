{ pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # ─── Bootloader ─────────────────────────────────────────────────────
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.configurationLimit = 10;

  # ─── Networking ─────────────────────────────────────────────────────
  networking.hostName = "silver";
  # Wired: the box lives on the USB-Ethernet dongle, so NetworkManager only ever
  # manages the cable. No Wi-Fi, so no agenix secret and no declarative profile.
  networking.networkmanager.enable = true;

  # ─── Time zone / locale ─────────────────────────────────────────────
  time.timeZone = "America/Regina";
  i18n.defaultLocale = "en_US.UTF-8";

  # ─── User ───────────────────────────────────────────────────────────
  users.users.yuanw = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "video"
    ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHUg80LmE2cirl2gPfmShkWZh68eIvlD6Uc3swGfcAwY me@yuanwang.ca"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFpYgmWwtRG7vlRbtWheYrtHl9E9qx84sdU+YlE8w+CZ me@yuanwang.ca"
    ];
  };
  security.sudo.wheelNeedsPassword = false;

  # ─── SSH ────────────────────────────────────────────────────────────
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
  };

  # ─── mDNS (Avahi) ───────────────────────────────────────────────────
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    publish = {
      enable = true;
      addresses = true;
      workstation = true;
    };
  };

  # ─── Telemetry │ Cockpit + PCP ──────────────────────────────────────
  # Caddy is deliberately NOT imported here yet: modules/caddy.nix serves
  # yuanw.me and local misfit services, and *.yuanw.me is routed to misfit by
  # the e8450. Moving that role here is a separate cutover. See
  # docs/nixos-macbook-pro-plan.org Step 6.
  services.cockpit-local = {
    enable = true;
    openFirewall = true;
  };
  services.pcp = {
    enable = true;
    preset = "standalone";
    openFirewall = true;
    allowedNetworks = [
      "127.0.0.0/8"
      "::1/128"
      "192.168.0.0/16"
      "10.0.0.0/8"
    ];
  };

  # ─── Swap ───────────────────────────────────────────────────────────
  zramSwap.enable = true;

  # ─── Nix ────────────────────────────────────────────────────────────
  nixpkgs.config.allowUnfree = true;
  nix.settings.trusted-users = [
    "root"
    "yuanw"
  ];
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  # ─── Mac hardware tweaks (profile covers the rest) ──────────────────
  # 11-5 is the dual-GPU 15" profile and, unlike 11-4, does not set this.
  hardware.graphics.enable = true;
  # No camera on a telemetry box, and its firmware is extracted from macOS.
  hardware.facetimehd.enable = false;

  # Headless: logind suspends on lid close by default. The ExternalPower variant
  # defaults to "user" (= follows this one) and Docked defaults to "ignore", so
  # this covers the closed-lid case on its own. shortcut: still suspends if
  # someone flips those back; add them if the box ever runs docked with the lid
  # closed.
  services.logind.settings.Login.HandleLidSwitch = "ignore";

  # ─── System packages ────────────────────────────────────────────────
  environment.systemPackages = with pkgs; [
    curl
    git
    htop
    tmux
    tree
    vim
    wget
    pciutils
    smartmontools
  ];

  system.stateVersion = "25.05";
}
