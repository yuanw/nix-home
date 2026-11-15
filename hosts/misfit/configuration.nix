# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{
  pkgs,
  config,
  inputs,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    # ./hardware-configuration.nix
    # "${builtins.fetchTarball "https://github.com/nix-community/disko/archive/master.tar.gz"}/module.nix"
    ./disk-config.nix
    # The qBittorrent seeder. Kept in the private repo because its serverConfig
    # holds the WebUI Password_PBKDF2 hash. Plan: nix-home-private/docs/
    # misfit-torrent.org.
    (inputs.nix-home-private + "/modules/arr.nix")
  ];

  nixpkgs.config.problems.handlers = {
    # bimmer-connected is used by home-assistant. Marked broken due to test
    # failures with Python 3.14 asyncio API changes. The overlay below disables
    # tests; this handler allows evaluation to proceed past the broken check.
    bimmer-connected.broken = "ignore";
  };

  nixpkgs.overlays = [
    (_final: prev: {
      python314Packages = prev.python314Packages.overrideScope (
        _pyfinal: pyprev: {
          bimmer-connected = pyprev.bimmer-connected.overrideAttrs (_: {
            doCheck = false;
          });
        }
      );
    })
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.configurationLimit = 5;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  i18n.defaultLocale = "en_US.UTF-8";

  networking.hostName = "misfit";

  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true; # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "America/Regina";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";
  networking.hostId = "b0dc1b84";
  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  services.openssh = {
    enable = true;
  };

  services.avahi = {
    enable = true;
    #https://github.com/georgewhewell/nixos-host/blob/329266870c2b382cc8a57bd7658e3e32752dc14c/services/home-assistant/default.nix
    # Reflect incoming mDNS requests to all allowed network interfaces.for homekit bridge
    settings.reflector.enable-reflector = true;
    openFirewall = true;

  };

  networking.firewall = {
    # 21063 homekit bridge
    allowedTCPPorts = [
      21064
      21066
      21067
      1400
      1900
    ];
    allowedUDPPorts = [
      21064
      21066
      21067
      5353
    ]; # 5353 is for mDNS/Bonjour discovery

  };
  services.home-assistant = {
    enable = true;
    package = pkgs.home-assistant.override {
      packageOverrides = _self: super: {
        # Tests fail with Python 3.14 asyncio API changes. Disable until upstream fixes.
        bimmer-connected = super.bimmer-connected.overrideAttrs (_: {
          doCheck = false;
        });
      };
    };
    extraArgs = [ "--debug" ];
    extraPackages =
      python3Packages: with python3Packages; [
        isal
        gtts
        aiohomekit
        getmac
        aiohttp-fast-zlib
        pychromecast
      ];

    extraComponents = [
      # Components required to complete the onboarding
      "apple_tv"
      "application_credentials"
      "auth"
      "backup"
      "bayesian"
      "bluetooth"
      "bmw_connected_drive"
      "buienradar"
      "camera"
      "command_line"
      "conversation"
      "default_config"
      "dsmr"
      "ebusd"
      "esphome"

      "forecast_solar"
      "fritz"
      "google_translate"
      "homekit"
      "homekit_controller"
      "http"
      "ibeacon"
      "lawn_mower"
      "met"
      "mqtt"
      "my"
      "ohme"
      "openweathermap"
      "ping"

      "prometheus"
      "proximity"

      "scrape"
      "sensor"
      "shopping_list"
      "smartthings"

      "skybell"
      "sun"
      "switchbot"
      "switchbot_cloud"
      "tasmota"
      "template"
      "tplink"
      "tplink_tapo"
      "utility_meter"
      "vacuum"
      "valve"
      "zha"

      # Recommended for fast zlib compression
      # https://www.home-assistant.io/integrations/isal

    ];
    config = {
      default_config = { };
      homeassistant = {
        unit_system = "metric";
        external_url = "https://ha.yuanw.me";
        internal_url = "https://ha.yuanw.me";
      };
      http = {
        use_x_forwarded_for = true;
        trusted_proxies = [
          "127.0.0.1"
          "::1"
        ];
      };
      device_tracker = [
        {
          platform = "luci";
          host = "192.168.1.1";
          username = "!secret openwrt_admin_username";
          password = "!secret openwrt_admin_password";
          # interval_seconds = 30; # instead of 12seconds
          # consider_home = 300; # 5 minutes timeout
          # new_device_defaults = {
          #   track_new_devices = true;
          # };
        }
      ];

    };
  };
  services.isponsorblocktv.enable = true;

  # Apple Watch -> HealthSave iOS app -> this API. Rootless podman units, all
  # binding loopback; the phone reaches it through the healthsave.yuanw.me
  # vhost in modules/caddy.nix. Data lives in its own ZFS dataset (see the
  # module), not in the ephemeral root that rolls back to @blank every boot.
  services.healthsave-observatory = {
    enable = true;
    envFile = config.age.secrets.healthsave-env.path;
  };

  age.secrets = {
    namecheap.file = ../../secrets/namecheap.age;
    # Jellarr's Jellyfin API key — the same value as `pass jellyfin/apiKey`,
    # which the /Library/Refresh calls already use. The old jellyfin-admin
    # secret (a Jellyfin password *hash* that only declarative-jellyfin's init
    # read) is unwired now; the .age file stays in secrets/ until jellarr's
    # `users` can take the admin user over, which needs the plaintext instead.
    jellarr-api-key.file = ../../secrets/jellarr-api-key.age;
    hass = {
      file = ../../secrets/hass.age;
      path = "${config.services.home-assistant.configDir}/secrets.yaml";
      owner = "hass";
      group = "hass";
    };
    # read only by healthsave-env.service (root), which renders the per-container
    # env files under /run/healthsave — the containers never need this file
    healthsave-env.file = ../../secrets/healthsave-env.age;

  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "me@yuanwang.ca";

    certs."yuanw.me" = {
      group = config.services.caddy.group;
      domain = "yuanw.me";
      extraDomainNames = [
        "*.yuanw.me"

      ];
      dnsProvider = "cloudflare";
      dnsResolver = "1.1.1.1:53";
      # nixpkgs passes `--ari-disable` on its fresh-order branch but not on the
      # renew branch, which instead sends an ARI `replaces` serial Let's Encrypt
      # no longer has a record of -- every renewal then dies with 400 malformed
      # and the cert quietly expires.  Drop this when renewOpts gets the flag:
      # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/security/acme/default.nix#L329
      extraLegoRenewFlags = [ "--ari-disable" ];
      #dnsPropagationCheck = false;
      dnsPropagationCheck = true;
      #webroot = "/var/lib/acme";
      environmentFile = config.age.secrets.namecheap.path;
    };
  };

  # only meaningful while jellyfin is enabled (see below)
  users.users.${config.services.jellyfin.user} = pkgs.lib.mkIf config.services.jellyfin.enable {
    extraGroups = [
      "video"
      "render"
    ];
  };

  # Jellyfin — the server from nixpkgs, the configuration from Jellarr below.
  # The 10.11.x pin in ./default.nix is now about the plugin ABI, not about the
  # config engine.
  #
  # Pin trap: plugins installed while 12.x ran are still in the data dir and
  # are ABI-incompatible with 10.11.x. TheTVDB_19.0.0.0 threw
  # MissingMethodException in TvdbClientManager's constructor during host
  # startup — fatal, the server never binds. It is parked out of the way in
  # /var/lib/jellyfin/plugins.disabled (not declarative: nixpkgs has no
  # plugin-install option here). Reinstall a 10.11-compatible build, or delete
  # it for good, before dropping this pin.
  services.jellyfin = {
    enable = true;
    group = "data";
  };

  # Jellarr applies the rest of the server configuration through Jellyfin's own
  # REST API (https://github.com/venkyr77/jellarr). It replaced
  # declarative-jellyfin, which wrote system.xml and jellyfin.db directly,
  # stopped and restarted the server several times per activation,
  # reimplemented Jellyfin's user-UUID generation in bash, and hard-asserted on
  # any Jellyfin version it had not been taught — that assertion is what forced
  # the pin, and the migration it ran at every activation is what the memory
  # cgroup below was added to contain.
  #
  # Jellarr only writes what it is given, so the rest of this host's UI state
  # stays as set. What is therefore *not* declared, each for a concrete reason:
  #   * serverName ("My Declarative Jellyfin Server") and UICulture = "en" —
  #     no option exists for either in jellarr's schema
  #   * the Movies library's metadataFetchers/imageFetchers (TMDb + OMDB) — its
  #     libraryOptions carries pathInfos and nothing else
  #   * the admin user `yuanw`: jellarr takes a *plaintext* password file and
  #     secrets/jellyfin-admin.age holds a hash, so declaring it needs a new
  #     secret first. Until then nothing here can re-create or reset that user.
  #   * the Open Subtitles plugin, still a reviewed fetchzip in the preStart
  #     below rather than a repo install of whatever is latest
  services.jellarr = {
    enable = true;
    # JELLARR_API_KEY. The secret is an EnvironmentFile, so its content is a
    # `JELLARR_API_KEY=<key>` line — a bare key parses to no assignment at all
    # and the unit exits with `Error: JELLARR_API_KEY required` (measured on the
    # first activation, 2026-11-14). systemd reads EnvironmentFile as root before
    # switching users, so agenix's default root:root 0600 is enough — unlike the
    # old jellyfin-init `cat`, which needed an owner override.
    environmentFile = config.age.secrets.jellarr-api-key.path;
    config = {
      version = 1;
      base_url = "http://localhost:8096";
      startup.completeStartupWizard = true;
      # use hardware acceleration for trickplay image generation
      system.trickplayOptions = {
        enableHwAcceleration = true;
        enableHwEncoding = true;
      };
      # The seed tree /data/torrents is deliberately not a library — it would
      # get release-group directory names scraped. Titles are hardlinked in.
      library.virtualFolders = [
        {
          name = "Movies";
          collectionType = "movies";
          libraryOptions.pathInfos = [ { path = "/data/Movies"; } ];
        }
        {
          name = "Shows";
          collectionType = "tvshows";
          libraryOptions.pathInfos = [ { path = "/data/Shows"; } ];
        }
      ];
    };
  };

  # The module only wires a daily timer (Persistent = true), so activation alone
  # would not converge anything and a fresh timer can wait a day. The oneshot is
  # cheap when the server is already in sync, and its preStart polls
  # /System/Info/Public for up to 120s, so it tolerates Jellyfin still booting.
  systemd.services.jellarr.wantedBy = [ "multi-user.target" ];

  # Contain jellyfin's startup: MemoryHigh throttles the runaway migration run
  # into swap (16G zvol) instead of letting it page through home-assistant and
  # caddy; MemoryMax is the last-resort ceiling. Tune via MEMORY_HIGH once
  # misfit's real RSS headroom is measured.
  #
  # Why it exists: the 12.x startup regression made declarative-jellyfin's init
  # (which booted the server once for DB migrations before touching them) OOM-
  # loop the box at ~27G RSS — https://github.com/Sveske-Juice/declarative-
  # jellyfin/issues/32. Nothing runs that init any more, so this is now belt and
  # braces; keep it until the 12.x behaviour is measured on this box.
  systemd.services.jellyfin.serviceConfig = {
    MemoryHigh = "6G";
    MemoryMax = "8G";
    # die before anything else on this box does
    OOMScoreAdjust = 500;
    # nixpkgs' unit already sets Restart=on-failure
    RestartSec = "30s";
    # jellyfin's unit doesn't set this; .NET would otherwise reserve
    # server-GC heaps per core and overshoot the cgroup cap
    Environment = "DOTNET_gcServer=0";
  };

  # Open Subtitles plugin.  jellarr *can* install plugins (`plugins = [ { name
  # = "Open Subtitles"; } ]` plus system.pluginRepositories), but that takes
  # whatever the repository lists as current, and this build is pinned to the
  # server ABI — so the reviewed zip stays here.
  # `preStart` runs as the unit User (jellyfin), which is what ends up owning
  # the directory and files created below.
  #
  # 24.0.0.0 is the last build for the 10.11 ABI (meta.json targetAbi
  # 10.11.8.0); 25.0.0.0 targets jellyfin 12 and would fail the way TheTVDB
  # did — see the pin note in ./default.nix.  Bumping the version means a new
  # directory name here *and* deleting the old one from the data dir, or
  # jellyfin loads both.  Jellyfin rewrites meta.json in place (status,
  # autoUpdate); cp overwriting it is fine, it comes back as Active.
  # The OpenSubtitles.com API key is UI-only state, set once in
  # Dashboard → Plugins → Open Subtitles.
  systemd.services.jellyfin.preStart =
    let
      # Spaced name, the layout jellyfin's own installer uses.
      pluginDir = "${config.services.jellyfin.dataDir}/plugins/Open Subtitles_24.0.0.0";
    in
    ''
      d='${pluginDir}'
      mkdir -p "$d"
      # cp preserves the store's read-only modes; only the owner (us) can undo them
      if [ -d "$d" ]; then chmod -R u+w "$d"; fi
      cp -aT ${
        pkgs.fetchzip {
          url = "https://repo.jellyfin.org/files/plugin/open-subtitles/open-subtitles_24.0.0.0.zip";
          sha256 = "sha256-b6sgmgBlvhUAhFuq0p/EjB3604NGBkpS4NP33n1hfKc=";
          stripRoot = false;
        }
      } "$d"
      # After, not only before: cp -a copies the store's 0444 onto the files, and
      # jellyfin rewrites meta.json (status, autoUpdate) as it loads the plugin.
      # With that mode it threw UnauthorizedAccessException on meta.json and took
      # the whole server down — "[FTL] Main: Error while starting server", every
      # activation, and the unit still reads active because the process stays up.
      # declarative-jellyfin's ExecStartPre (chmod -R 750 over the data dir) used
      # to undo this behind our back; nothing does now.
      chmod -R u+w "$d"
    '';

  # The two Jellyfin library roots, group-writable for the humans only.
  # Without these, `mkdir '/data/Movies/Title (2021)'` fails for yuan and yuanw
  # (both roots were root:root 0755 since the dataset was created), so adding a
  # title to the library needs sudo.
  #
  # `users` and not `data`, deliberately: jellyfin's primary gid *is* `data`
  # (services.jellyfin.group above), so a group-writable library would hand
  # write access on the whole media library to a network-facing web server.
  # `users` (gid 100) has exactly two members, yuan and yuanw, and no service.
  # Setgid so a new title directory inherits the group instead of the creator's
  # primary group. Existing content is left alone — it is yuanw:users 0644 and
  # Jellyfin reads it world-readable, so nothing needs a chgrp -R.
  systemd.tmpfiles.rules = [
    "d /data/Movies 2775 root users - -"
    "d /data/Shows 2775 root users - -"
  ];

  # Prowlarr — the search layer for the seeder (plan: nix-home-private/docs/
  # misfit-torrent.org, Phase 4). Without it every grab still starts in a
  # browser.
  #
  # Public on purpose, unlike the qBittorrent seeder in the private repo: the
  # Nix here holds no credential. Indexers, their API keys and the qBittorrent
  # download-client entry (127.0.0.1:8080, the Phase 2 user and password) are
  # UI-only state in Prowlarr's own DB. `settings` is free-form INI and lands
  # world-readable in the store, so anything secret goes through
  # `environmentFiles` as PROWLARR__SECTION__KEY=value lines instead.
  services.prowlarr = {
    enable = true;
    # openFirewall stays at its default false: the servarr UI has no auth out
    # of the box, and a closed 9696 means reaching it as
    #   ssh -N -L 9696:localhost:9696 misfit
    # Prowlarr -> qBittorrent is loopback (127.0.0.1:8080), which never touches
    # the firewall. Reopen only if a phone has to drive the UI.
  };

  # DynamicUser + StateDirectory put the real tree at /var/lib/private/prowlarr,
  # and /var/lib/private is already in the /persist list above — so unlike the
  # seeder's /var/lib/qBittorrent this needs no entry. Unverified: the reboot
  # proof (indexers and the qBittorrent credential survive) is still owed.

  # Not by default. One line, added the first time an indexer answers 503 or a
  # Cloudflare interstitial:
  # services.flaresolverr.enable = true;   # Prowlarr reaches it on 127.0.0.1:8191

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # services.pulseaudio.enable = true;
  # OR
  # services.pipewire = {
  #   enable = true;
  #   pulse.enable = true;
  # };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.libinput.enable = true;

  # security.sudo.extraRules = [
  #   {
  #     users = [ "yuanw" ];
  #     commands = [
  #       {
  #         command = "/run/current-system/sw/bin/rsync";
  #         options = [ "NOPASSWD" ];
  #       }
  #     ];
  #   }
  # ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  security.sudo = {
    wheelNeedsPassword = false;
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHUg80LmE2cirl2gPfmShkWZh68eIvlD6Uc3swGfcAwY me@yuanwang.ca"
  ];

  users.groups.data = { };
  users.users.yuan = {
    isNormalUser = true;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMSvr2qkdnG03/pGLo3aCFTnwmvojKO6m/W74ckC1RPW me@yuanwang.ca"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHUg80LmE2cirl2gPfmShkWZh68eIvlD6Uc3swGfcAwY me@yuanwang.ca"
      # silver's nix daemon logs in as yuan to build x86_64-linux closures here
      # (docs/harmonia-build-hub.org 3.1); the private half is
      # secrets/nix-builder-key.age on silver.
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeGertBlb+dywvvhHD4vpxH3D7B85q7jkzWSKatVIbJ silver-build-hub"
      # mist's own daemon logs in as yuan as well, to remote-build x86_64-linux drvs
      # it cannot evaluate (hosts/mist.nix nix-builders).  Private half is
      # /etc/nix/misfit-builder-key on mist, deliberately not in the repo.
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJUOVHRIj0Ssus/nrgkatoVcyCi3iPbTNm9E8ElRvi56 mist-nix-builder"
    ];
    extraGroups = [
      "wheel"
      "data"
      "docker"
    ]; # Enable ‘sudo’ for the user.
    packages = with pkgs; [
      tree
      git
      vim
      nix-diff
      lego
      fastfetch
      pciutils
      bluez-experimental
      nmap
      isponsorblocktv
    ];
  };

  environment = {
    systemPackages = with pkgs; [
      wget
      vim
      git
    ];
    shells = [ pkgs.zsh ];
  };

  # Ethernet drivers for stage-1. The wired uplink enp3s0 is an onboard
  # RTL8125B driven by the in-tree r8169 driver (the old "cannot load this
  # firmware" note was wrong: r8169 loads fine — stage-2 runs this driver
  # today and links up even while the optional rtl_nic/rtl8125b-2.fw request
  # fails with -2; that fw file exists nowhere in nixpkgs). Without the
  # module copied+loaded in the initrd, stage-1 has NO NIC at all, so the
  # initrd-ssh unlock shell on port 2222 is unreachable = dead-end boot.
  # igc/i40e (the SFP cards) are idle; harmless, keep them listed.
  boot.initrd.kernelModules = [ "r8169" ]; # modprobed at stage-1 start (modules-load.d)
  boot.initrd.availableKernelModules = [
    "igc"
    "i40e"
    "r8169"
  ]; # copied into the initrd image
  boot.zfs.forceImportRoot = false;
  # Remote-unlock support for the encrypted zroot:
  # - keep asking for the pool passphrase long enough (default 0 = wait
  #   forever; 1800 = finite) that I can answer from an initrd-ssh shell:
  #   ssh -p2222 root@misfit  ->  echo PASSPHRASE | zfs load-key zroot/root
  #   (if the import unit stalls, re-run it from that shell: systemctl restart
  #   zfs-import-zroot.service — the second pass skips datasets already
  #   unlocked and finishes, then rollback + mounts proceed)
  # - without the ordering below, the import unit (DefaultDependencies=no,
  #   after=[modules-load ask-password-console] only) starts racing sshd and
  #   networkd: the prompt can time out while nobody can reach the machine.
  boot.zfs = {
    requestEncryptionCredentials = true;
    passwordTimeout = 1800;
  };

  boot.kernelParams = [
    # DHCP on all NICs in stage-1 (systemd-network-generator). Old value
    # "ip=::::nixos-initrd::dhcp" had an empty DEVICE field = may have
    # configured no interface at all; there is no .network file in the image,
    # so ip= is the only thing that could configure enp3s0.
    "ip=dhcp"
    # cap ZFS ARC at 4 GiB so userspace (jellyfin, hass) has headroom
    "zfs.zfs_arc_max=4294967296"
  ];

  # give network + sshd a chance to come up before the pool-unlock prompt
  # waits on a human; keep the module's own deps + add sshd/networkd (safe
  # whatever the list-merge semantics; duplicates are harmless)
  boot.initrd.systemd.services.zfs-import-zroot = {
    wants = [
      "sshd.service"
      "systemd-networkd.service"
    ];
    after = [
      "systemd-modules-load.service"
      "systemd-ask-password-console.service"
      "sshd.service"
      "systemd-networkd.service"
      "systemd-networkd-wait-online.service"
    ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = 3600;
    };
  };
  # systemd stage 1 replaces postDeviceCommands; roll back the ephemeral
  # root dataset to @blank after the pool is imported, before sysroot mounts
  boot.initrd.systemd.services.zfs-rollback = {
    description = "Roll back the root ZFS dataset to @blank";
    wantedBy = [ "initrd.target" ];
    requires = [ "zfs-import-zroot.service" ];
    after = [ "zfs-import-zroot.service" ];
    before = [ "sysroot.mount" ];
    unitConfig.DefaultDependencies = "no";
    serviceConfig.Type = "oneshot";
    path = [ config.boot.zfs.package ];
    script = ''
      zfs rollback -r zroot/root@blank
    '';
  };

  fileSystems."/persist".neededForBoot = true;
  fileSystems."/persistSave".neededForBoot = true;
  fileSystems."/sshkeys".neededForBoot = true;

  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/home"
      "/var/log"
      "/var/lib/nixos"
      "/var/lib/private"
      "/etc/nixos"
    ];
    files = [
      "/etc/machine-id"
    ];
  };

  environment.persistence."/persistSave" = {
    hideMounts = true;
    directories = [
      "/var/lib/acme"
      "/var/lib/caddy"
      "/var/lib/hass"
      "/var/lib/jellyfin"
      "/var/lib/isponsorblocktv"
      "/etc/secrets"
    ];
  };

  # ─── ZFS snapshots (sanoid) ─────────────────────────────────────────
  # Retention for the two persistence datasets and the media library.
  # Deliberately *not* managed here:
  #   zroot/root   — ephemeral, rolled back to @blank every boot, so a
  #                  sanoid snapshot of it is always stale by definition.
  #                  (@blank stays ours: sanoid only ever prunes snapshots
  #                  named autosnap_*, so a foreign name is invisible to it
  #                  — checked against the sanoid 2.3.0 source.)
  #   zroot/root/nix  — the store is rebuildable, snapshots buy nothing
  #   zroot/root/swap — zvol, nothing to roll back to
  services.sanoid = {
    # hourly snap+prune pass (module default interval = "hourly")
    enable = true;

    templates.default = {
      autosnap = true;
      autoprune = true;
      hourly = 24;
      daily = 14;
      monthly = 6;
    };

    # Media library: mostly static, so hourlies add metadata with no
    # recovery value. 30 daily + 12 monthly covers "someone deleted an
    # episode" without eating the pool.
    templates.media = {
      autosnap = true;
      autoprune = true;
      hourly = 0;
      daily = 30;
      monthly = 12;
    };

    datasets = {
      "zroot/root/persist".useTemplate = [ "default" ];
      "zroot/root/persistSave".useTemplate = [ "default" ];
      # PHI gets its own dataset (services.healthsave-observatory.dataset) so
      # it can be snapshotted, cloned or zfs-sent without the rest of /persistSave
      "zroot/root/persistSave/healthsave".useTemplate = [ "default" ];
      "zroot/data".useTemplate = [ "media" ];
    };
  };

  services.openssh.hostKeys = [
    {
      path = "/sshkeys/ssh_host_ed25519_key";
      type = "ed25519";
    }
    {
      path = "/sshkeys/ssh_host_rsa_key";
      type = "rsa";
      bits = 4096;
    }
  ];

  boot.initrd.network = {
    enable = true;
    ssh = {
      enable = true;
      port = 2222;
      # Throwaway initrd-only host key (NEVER reuse /sshkeys production host
      # keys — they would sit on the unencrypted boot disk; nixpkgs warns).
      # An absolute *string* makes initrd-secrets copy it from misfit's own
      # filesystem at activation, so no build host needs the file in its tree.
      # A path literal (`../../secrets/...`) only ever worked when the builder
      # had that untracked file rsynced in: a git-based flake source drops
      # .gitignore'd files, so the initrd shipped an sshd with no host key and
      # remote boot was dead on arrival (and `colmena apply` fails at
      # install-bootloader with "failed to create initrd secrets!").
      hostKeys = [ "/etc/secrets/initrd/ssh_host_ed25519_key" ];
      authorizedKeys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMSvr2qkdnG03/pGLo3aCFTnwmvojKO6m/W74ckC1RPW me@yuanwang.ca"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHUg80LmE2cirl2gPfmShkWZh68eIvlD6Uc3swGfcAwY me@yuanwang.ca"
      ];

    };
  };
  # programs.firefox.enable = true;

  # List packages installed in system profile.
  # You can use https://search.nixos.org/ to find more packages (and options).
  # environment.systemPackages = with pkgs; [
  #   vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #   wget
  # ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # does not work with flake
  # system.copySystemConfiguration = true;

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    settings = {
      General = {
        Experimental = true; # Show battery charge of Bluetooth devices
      };
    };
  };
  nix = {
    # package = pkgs.nixVersions.git;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 10d";
    };
    optimise = {
      automatic = true;
      dates = [ "weekly" ];
    };
    extraOptions = ''

      experimental-features = nix-command flakes

    '';
    settings = {
      auto-optimise-store = true;
      allowed-users = [
        "root"
        "yuan"
      ];
      trusted-users = [
        "root"
        "yuan"
      ];
    };
  };

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  #
  # The Phase 5.8 IPsec lab peer lived here -- strongswan-swanctl with a lab.conf
  # pointing at 10.1.0.1, an esp accept rule, 500/4500/5201, and a unit putting
  # 10.90.0.2/32 on enp4s0. Removed 2026-10-17 with the cutover, as that block
  # always said to do; the gateway's half was already gone and the kernel patches
  # in hosts/mono-gateway/default.nix are unrelated to this tunnel, so keep them.

  system.stateVersion = "25.05"; # Did you read the comment?
}
