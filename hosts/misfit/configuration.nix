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
    # which the /Library/Refresh calls already use. The old jellyfin-admin secret
    # (a Jellyfin password *hash* that only declarative-jellyfin's init read) was
    # deleted 2026-11-15: nothing wired it after the move to jellarr, and jellarr's
    # `users` would need the plaintext regardless, so it could never have taken the
    # admin account back.
    jellarr-api-key.file = ../../secrets/jellarr-api-key.age;
    # WireGuard private key for the seeder's Mullvad exit. The account number is
    # not in here and never was — the device is registered, and the peer key is
    # Mullvad's public relay key, so this file is the only secret involved.
    mullvad-wg-key.file = ../../secrets/mullvad-wg-key.age;
    hass = {
      file = ../../secrets/hass.age;
      path = "${config.services.home-assistant.configDir}/secrets.yaml";
      owner = "hass";
      group = "hass";
    };
    # read only by healthsave-env.service (root), which renders the per-container
    # env files under /run/healthsave — the containers never need this file
    healthsave-env.file = ../../secrets/healthsave-env.age;

    # The arr stack.  All four are read by systemd as root — as EnvironmentFile
    # for the servarr units and for configarr, and through Bazarr's
    # LoadCredential — so agenix's default 0600 root:root is right and no
    # service ever needs one.  (*-api-key are the bare keys for Bazarr; *-auth
    # are `NAME=value` bodies.  See secrets/secrets.nix for why both exist.)
    radarr-auth.file = ../../secrets/radarr-auth.age;
    sonarr-auth.file = ../../secrets/sonarr-auth.age;
    radarr-api-key.file = ../../secrets/radarr-api-key.age;
    sonarr-api-key.file = ../../secrets/sonarr-api-key.age;
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
  #   * the admin user `yuanw`: jellarr takes a *plaintext* password file and the
  #     old secrets/jellyfin-admin.age held only a hash (deleted 2026-11-15), so
  #     declaring it needs a new secret first. Until then nothing here can
  #     re-create or reset that user.
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
  # cheap when the server is already in sync.
  systemd.services.jellarr.wantedBy = [ "multi-user.target" ];

  # …but it races Jellyfin, and the module's own readiness probe is too weak:
  # it polls /System/Info/Public, which answers as soon as Kestrel binds, while
  # core startup takes another ~4s ("Core startup complete") and every admin
  # endpoint answers 503 until then. Measured on the first activation: jellarr
  # ran about a second after jellyfin's ExecStart and died at `GET
  # /System/Configuration failed: 503`. Retry until the server is really up,
  # and keep the start limit out of the way so a slow boot cannot strand the
  # unit in failed state before the daily timer would have run anyway.
  systemd.services.jellarr.serviceConfig = {
    Restart = "on-failure";
    RestartSec = "10s";
  };
  systemd.services.jellarr.unitConfig.StartLimitIntervalSec = 0;

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
    # 2775 + group `media` (users.groups.media below, and the plan's [[#group]]):
    # Radarr/Sonarr create the title directories here, and the setgid bit is what
    # lets a title dir inherit `media` instead of the importer's primary group.
    # The comment that chose `users` over `data` in 2026-10-31 still holds for
    # the part it argued — jellyfin must not be able to write the library — and
    # `media` keeps that property while adding the importers.
    #
    # No chgrp -R of the existing content: the 1746 files stay yuanw:users 0644,
    # which jellyfin reads world-readable as it always has. Where an importer
    # needs to touch one of those titles, chgrp that title's directory, then.
    "d /data/Movies 2775 root media - -"
    "d /data/Shows 2775 root media - -"
  ];

  # Mullvad, scoped to the seeder's uid — Phase 5 of nix-home-private/docs/
  # misfit-torrent.org. Read that page before changing this: Mullvad removed port
  # forwarding in 2023, so this buys outbound privacy and gives up inbound peers
  # (qBittorrent will report `connection_status: firewalled`). That is the deal, not
  # a bug to chase. Everything else on the box keeps leaving via the home IP.
  #
  # Values come from measurement, not guesswork: us-sea-wg-401 is the lowest-RTT
  # relay from misfit (~25ms vs ~32ms Toronto, probed 2026-11-15), and the tunnel
  # addresses are what Mullvad's own API assigned to this device.
  my.mullvad = {
    enable = true;
    privateKeyFile = config.age.secrets.mullvad-wg-key.path;
    address = [
      "10.68.193.171/32"
      "fc00:bbbb:bbbb:bb01::5:c1aa/128"
    ];
    relayHost = "23.234.80.2";
    relayPublicKey = "wRvkGNE3N2UklxKajU06gbBJ3Bg7KmhZsU7a5HIFBw8=";
  };

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
    # openFirewall stays at its default false. Two ways in, neither through 9696:
    #   ssh -N -L 9696:localhost:9696 misfit   (API/scripts, no TLS)
    #   https://prowlarr.yuanw.me              (Caddy, modules/caddy.nix)
    #
    # Bound to loopback on purpose. Out of the box Prowlarr listens on *:9696 with
    # no authentication, so the closed firewall rule was the *only* thing between a
    # LAN host and an arr UI that can edit indexers and download clients. Binding it
    # means Caddy on 127.0.0.1 is the only reachable path.
    settings.server = {
      bindAddress = "localhost";
      # Honor X-Forwarded-For from Caddy, or every request looks like it came from
      # 127.0.0.1 and the logs are useless.
      isProxied = true;
      # "external" — the default, stated so the reason is on the record.
      #
      # This cannot be "none" even though Caddy in front of it already authenticates.
      # Prowlarr's UI refuses to run without its own account: with authentication
      # disabled it serves a forced setup page ("To prevent remote access without
      # authentication, Prowlarr now requires authentication to be enabled") instead
      # of the Indexers view. Verified in a browser, not inferred from a config key.
      #
      # And the account itself cannot be declarative: users live in prowlarr.db, while
      # everything above only reaches config.xml (via PROWLARR__SERVER__* environment
      # variables, which is why config.xml still reads <BindAddress>* while the socket
      # is 127.0.0.1 — check the socket and the env, not that file). So the account is
      # created exactly once in the browser, like the indexers and the download client,
      # and its password belongs in `pass`, never here.
      #
      # API automation is unaffected either way: X-Api-Key still authorizes /api/*.
      # "forms" = the login page. Not "external", even though that is the default and
      # sounds like the right answer for a reverse-proxied app.
      #
      # With "external" and no account, there is no login route at all: the request to
      # /api/v1/login is served by the API-key scheme and challenged, so every attempt
      # 401s no matter what password you send. Measured — the hash in the database
      # matched the password exactly (pbkdf2-sha512, 10000 iterations, 32-byte key) and
      # the log still said "ApiKeyAuthenticationHandler: AuthenticationScheme: API was
      # challenged". "forms" is what registers the login page and its handler.
      #
      # This has to be set here, not just picked in the UI: nixpkgs passes it as the
      # PROWLARR__SERVER__AUTHENTICATION environment variable, which outranks whatever
      # the General settings form writes. Pick it in the UI, it appears to take, and the
      # next restart silently reverts to whatever this line says.
      #
      # The account itself cannot be declarative — users live in prowlarr.db, and
      # Prowlarr refuses to serve its UI without one — so it is created once in the
      # browser and its password lives in pass:prowlarr/admin-password, never here.
      authentication = "forms";
    };
  };

  # DynamicUser + StateDirectory put the real tree at /var/lib/private/prowlarr,
  # and /var/lib/private is already in the /persist list above — so unlike the
  # seeder's /var/lib/qBittorrent this needs no entry. Unverified: the reboot
  # proof (indexers and the qBittorrent credential survive) is still owed.

  # Not by default. One line, added the first time an indexer answers 503 or a
  # Cloudflare interstitial:
  # services.flaresolverr.enable = true;   # Prowlarr reaches it on 127.0.0.1:8191

  # ─── The arr stack: Radarr, Sonarr, Bazarr, configarr ─────────────────────
  #
  # Plan: nix-home-private/docs/misfit-arr.org — read it before changing any of
  # this. The phases, the acceptance measurements and the reasoning live there;
  # this block carries what cannot be re-derived from it.
  #
  # The shape: Prowlarr searches, qBittorrent grabs into /data/torrents, Radarr
  # and Sonarr hardlink the result into the Jellyfin library and notify it. What
  # is *not* here is what each app keeps in its own DB — the admin account, the
  # qBittorrent download client (http://10.240.0.2:8080, because the daemon has
  # had no host loopback since Phase 5 of the torrent plan), the Prowlarr app
  # entries — same split as services.prowlarr above.

  # One group for everything the importers must write: the seed tree (a hardlink
  # out of it needs READ *and* WRITE on the source file under
  # fs.protected_hardlinks, and Radarr unlinks from it when a download fails) and
  # the two library roots (they mkdir in them).
  #
  # Not `data`: jellyfin's primary gid is `data` (services.jellyfin.group), so a
  # group-writable library would hand a network-facing web server the ability to
  # delete the whole media library. Not `users` either — that is the human group
  # holding yuan and yuanw, and four daemons in it is the same mistake aimed the
  # other way. `jellyfin` joins nothing: it reads everything world-readable, the
  # way it already reads the 1746 files that predate all of this.
  users.groups.media = { };

  # The nixpkgs modules create these users (User=/Group=, not DynamicUser), so
  # declaring membership here merges with their definitions. Deliberately not
  # done for a user no module creates: `users.users.yuanw` is the lesson — an
  # account with isNormalUser/group unset fails the build.
  users.users.radarr.extraGroups = [ "media" ];
  users.users.sonarr.extraGroups = [ "media" ];

  # Both servarr units hard-set UMask = "0022" (servarr/radarr.nix:96). With the
  # setgid library roots below that would make every title directory Radarr
  # creates 0755, and then Bazarr cannot write a .srt into it — so the umask is
  # part of the group model, not a cosmetic choice. mkForce because the module
  # sets the key rather than leaving it at its default.
  systemd.services.radarr.serviceConfig.UMask = pkgs.lib.mkForce "0002";
  systemd.services.sonarr.serviceConfig.UMask = pkgs.lib.mkForce "0002";

  # The activation that *introduces* a persistence mount is the one tmpfiles
  # misses — systemd-tmpfiles-setup ran at the previous boot, when no such mount
  # existed, and leaves a root-owned directory behind. That cost a whole deploy
  # of the seeder (nix-home-private/modules/arr.nix explains it for
  # /var/lib/qBittorrent), so the two new state trees do not repeat the
  # discovery. Every level is named: `install -d -o X -g X a/b/c` chowns only the
  # operand it is handed, and a root-owned parent stops mkpath() cold.
  systemd.services.radarr.serviceConfig.ExecStartPre =
    "+${pkgs.coreutils}/bin/install -d -m700 -o radarr -g radarr"
    + " /var/lib/radarr /var/lib/radarr/.config /var/lib/radarr/.config/Radarr";
  systemd.services.sonarr.serviceConfig.ExecStartPre =
    "+${pkgs.coreutils}/bin/install -d -m700 -o sonarr -g sonarr"
    + " /var/lib/sonarr /var/lib/sonarr/.config /var/lib/sonarr/.config/NzbDrone";

  # Replaces the module's RequiresMountsFor (a plain list — setting it here
  # overwrites it), so the data directory is named as well as /data. An importer
  # that starts before /data is mounted writes its library into the root dataset
  # and nobody notices until the pool is full.
  systemd.services.radarr.unitConfig.RequiresMountsFor = [
    "/data"
    "/var/lib/radarr"
  ];
  systemd.services.sonarr.unitConfig.RequiresMountsFor = [
    "/data"
    "/var/lib/sonarr"
  ];

  services.radarr = {
    enable = true;
    # Loopback, and openFirewall stays false: these UIs edit download clients and
    # move files, so the bind is the gate. Unlike Prowlarr this is not yet the
    # whole story — there is no account in the app until it is created, so
    # nothing may be published until `authentication` is set below (see the
    # services.prowlarr block for what "external with no user" actually means).
    settings.server = {
      bindAddress = "localhost";
      # Honour X-Forwarded-For from Caddy; without it every request is logged as
      # 127.0.0.1 and the CSRF check has nothing to compare.
      isProxied = true;
    };
  };

  services.sonarr = {
    enable = true;
    settings.server = {
      bindAddress = "localhost";
      isProxied = true;
    };
  };

  # Authentication, and why it is here rather than clicked in the UI.
  #
  # Both apps are past the point where the servarr UIs had a user table: there
  # is no /api/v3/user on either (measured — 404 with an admin API key, while
  # /api/v3/system/backup answers 200 with the same key), so an account is not
  # state in the app's DB the way Prowlarr's is.  It is one username and one
  # password in the app's config, which means it *can* be declarative, and the
  # password reaches it the way every other servarr secret does: an
  # EnvironmentFile, never `settings` (which is world-readable store content).
  #
  # "forms" for the same reason Prowlarr ended there: with the default
  # "external" and no reverse-proxy header to trust, nothing authenticates.  The
  # keys are the app's own config names uppercased into SERVARR__SECTION__KEY,
  # which is how nixpkgs renders `settings` (servarr/settings-options.nix:60);
  # verified by an unauthenticated request returning 401 and a logged-in one
  # returning 200, not by reading config.xml — which omits any key equal to its
  # default and so shows no <Auth> block at all.
  services.radarr.settings.auth = {
    method = "forms";
    required = "enabled";
    username = "yuan";
  };
  services.sonarr.settings.auth = {
    method = "forms";
    required = "enabled";
    username = "yuan";
  };
  services.radarr.environmentFiles = [ config.age.secrets.radarr-auth.path ];
  services.sonarr.environmentFiles = [ config.age.secrets.sonarr-auth.path ];

  # Subtitles.  The only app of the four whose module has a real secret channel
  # (`_secret` → LoadCredential → a jq pass into DYNACONF_*, bazarr.nix:36-70),
  # so unlike the others it is fully declarative — but the *names* below are its
  # own config.yaml keys and the module declares none of them (freeform type):
  # verified against the file it writes on first start, because an unknown
  # DYNACONF_* name is ignored rather than rejected.
  services.bazarr = {
    enable = true;
    settings = {
      general = {
        port = 6767;
        bind_address = "127.0.0.1";
      };
      sonarr.sonarr1 = {
        ip = "127.0.0.1";
        port = 8989;
        apikey._secret = config.age.secrets.sonarr-api-key.path;
      };
      radarr.radarr1 = {
        ip = "127.0.0.1";
        port = 7878;
        apikey._secret = config.age.secrets.radarr-api-key.path;
      };
    };
  };

  # configarr — TRaSH-Guides custom formats, quality definitions and profiles
  # into the two apps.  A oneshot on a timer, not a daemon: it converges and
  # exits, which is also what makes it safe to run by hand while debugging
  # (DRY_RUN=true is the dry-run switch; it takes no CLI flags at all).
  #
  # The package is ours (packages/configarr.nix) because nixpkgs has no
  # configarr; the unit is ours because nothing ships one.
  environment.etc."configarr/config.yml".source = ./configarr/config.yml;

  systemd.services.configarr = {
    serviceConfig = {
      Type = "oneshot";
      DynamicUser = true;
      # Where it clones TRaSH-Guides and the recyclarr template repo — src/env.ts
      # puts them at $ROOT_PATH/repos, so ROOT_PATH has to be the writable one.
      # /var/lib/private is on the /persist list, so the clones survive a boot;
      # losing them would only cost a re-clone.
      StateDirectory = "configarr";
      WorkingDirectory = "/var/lib/configarr";
      Environment = [
        "ROOT_PATH=/var/lib/configarr"
        "CONFIG_LOCATION=/etc/configarr/config.yml"
        "LOG_LEVEL=info"
        # The binary shells out to git (simple-git), and a service's PATH is not
        # the interactive one.  Named explicitly rather than trusted: the failure
        # this avoids is a green run that synced nothing but the log.
        "PATH=${
          pkgs.lib.makeBinPath [
            pkgs.git
            pkgs.coreutils
          ]
        }"
      ];
      # The same two files the apps start with, which is how config.yml's
      # `!env RADARR__AUTH__APIKEY` resolves.  It also hands configarr the two
      # passwords it does not need — systemd read them as root either way, and
      # splitting the files again to avoid that buys a fourth secret to rotate.
      EnvironmentFile = [
        config.age.secrets.radarr-auth.path
        config.age.secrets.sonarr-auth.path
      ];
      # callPackage, not pkgs.configarr: the packages/ overlay is only
      # applied to silver, asche and dgx-spark (hosts/colmena.nix:18,23) — misfit
      # gets its packages from the plain hive nixpkgs.
      ExecStart = "${pkgs.callPackage ../../packages/configarr.nix { }}/bin/configarr";
      # No network sandbox: it clones two GitHub repos and talks to two local
      # apps.  Everything else of the usual hardening set is inert for a
      # oneshot that writes only its StateDirectory.
      NoNewPrivileges = true;
      ProtectHome = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ReadWritePaths = [ "/var/lib/configarr" ];
    };
  };

  systemd.timers.configarr = {
    wantedBy = [ "timers.target" ];
    partOf = [ "configarr.service" ];
    timerConfig = {
      # Daily, like recyclarr's default.  Cheap (two GitHub clones + a few
      # hundred API calls) and the guides move more often than a week.
      OnCalendar = "*-*-* 04:15:00";
      Persistent = true;
      RandomizedDelaySec = "10m";
    };
  };

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
      # the seed tree + library roots (users.groups.media, and the arr-stack
      # block above): keeps the manual `ln` path working without sudo
      "media"
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
      # The scan an import needs once a hardlink lands — the last step of the
      # search→grab→ln flow (docs/misfit-torrent.org, "search last mile").
      # Packaged the house way (scripts/ + writeShellScriptBin, as hosts/mist.nix
      # does for pi-review). Only the secret path is interpolated, so renaming the
      # secret fails at eval time instead of quietly answering 401 mid-import; the
      # port is not a Nix option at this rev (8096 appears only as a firewall
      # default — Jellyfin keeps it in its own DB), so the script defaults to
      # loopback:8096 and takes JELLYFIN_URL if that ever changes. Run as root:
      # the key file is 0600.
      (pkgs.writeShellScriptBin "jellyfin-rescan" ''
        export KEY_FILE='${config.age.secrets.jellarr-api-key.path}'
        ${builtins.readFile ../../scripts/jellyfin-rescan}
      '')
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
      # The arr stack's state. These modules use User=/Group= + tmpfiles, not
      # DynamicUser + StateDirectory, so /var/lib/private does not cover them and
      # an omitted entry means the @blank rollback silently returns them to a
      # first-run wizard (the seeder's /var/lib/qBittorrent is the precedent, in
      # nix-home-private/modules/arr.nix). /var/lib/bazarr is listed before
      # Bazarr is enabled because the entry is free and the absence is not.
      "/var/lib/radarr"
      "/var/lib/sonarr"
      "/var/lib/bazarr"
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
