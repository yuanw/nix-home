{ pkgs, config, ... }:

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
  # No nix.gc here: services.harmonia-cache brings services.harmonia.gc, which
  # replaces it with the same 30d retention plus an ensureFree floor.  Running
  # both is a declared nixpkgs warning.

  # ─── Binary cache │ Harmonia ────────────────────────────────────────
  # Serves this host's /nix/store to the LAN on :5000, signed with the silver-1
  # key (secrets/harmonia-sign-key.age).  LAN only: harmonia has no auth and no
  # TLS.  Plan: docs/harmonia-build-hub.org Phase 1.
  services.harmonia-cache.enable = true;

  # ─── Build hub │ the three LAN builders ─────────────────────────────
  # silver schedules, mist/misfit/dgx-spark compile (Phase 3).  Variant A: the
  # Mac evaluates (it has the private flake input), copies the .drv closure
  # here, and `nix-store --realise --max-jobs 0` dispatches each drv to the
  # builder matching its system and copies the outputs back for Harmonia to
  # sign.  No local compile is suppressed by a global max-jobs = 0: that would
  # make `just remote-apply silver`, which builds on the target, depend on
  # misfit being up.  The trigger passes --max-jobs 0 instead.
  #
  # publicHostKey is the base64 of the whole `ssh-ed25519 AAAA…` line, not the
  # bare blob — nix base64-decodes it into a single-key known_hosts file and
  # passes -oUserKnownHostsFile (verified against nix 2.34.9's ssh.cc, and by
  # connecting with and without a wrong key).  It is what stops a stale DHCP
  # lease from becoming a builder, so it is not optional.
  age.secrets.nix-builder-key.file = ../../secrets/nix-builder-key.age;

  nix = {
    distributedBuilds = true;

    # silver sends the inputs, so the builder builds from what the hub holds
    # rather than pulling its own copies (and so the private input source can
    # reach a builder that has no credentials for it).
    settings.builders-use-substitutes = false;

    buildMachines = [
      {
        hostName = "mist.lan";
        system = "aarch64-darwin";
        protocol = "ssh-ng";
        sshUser = "yuan";
        sshKey = config.age.secrets.nix-builder-key.path;
        # mist is also the control box and sleeps; it is opportunistic, so it
        # gets a speedFactor that loses every tie against misfit.
        maxJobs = 6;
        speedFactor = 2;
        publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUxscWxjZ1pXMDVEN0F1TmZiMWR5eUgwWHJDZElXR0JRWWd3L0tNamNVZFA=";
      }
      {
        hostName = "misfit.lan";
        system = "x86_64-linux";
        protocol = "ssh-ng";
        sshUser = "yuan";
        sshKey = config.age.secrets.nix-builder-key.path;
        maxJobs = 8;
        speedFactor = 4;
        publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUR4K0RhMEdLNVBQM2hza1luTG4xemQvMmlQbkxNS2wwUzZRMm8rZi9ERVE=";
      }
      {
        hostName = "dgx-spark.lan";
        system = "aarch64-linux";
        protocol = "ssh-ng";
        sshUser = "yuanw";
        sshKey = config.age.secrets.nix-builder-key.path;
        # Inference box: 20 cores, 81G already resident in a model.  One job.
        maxJobs = 1;
        speedFactor = 1;
        publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUJsZjhpVi9VK0lPMjJQR2M2eVpNYmZ3WkI3V3BrN0w3bkQ5QVpBcHdRQlM=";
      }
    ];
  };

  # `nix-store --realise --add-root /nix/var/nix/gcroots/cache/$t --indirect`
  # creates the symlink client-side, so the trigger's user has to be able to
  # write the directory: yuanw is in wheel (verified: without this the first
  # realise died with "creating directory … Permission denied").  Inside
  # /nix/var/nix/gcroots, so the standard gcroots scan and Harmonia's GC both
  # see the roots without services.harmonia.gc.gcRootsDirs.
  systemd.tmpfiles.rules = [
    "d /nix/var/nix/gcroots/cache 0775 root wheel - -"
  ];

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
