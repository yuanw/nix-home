# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{
  pkgs,
  config,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    # ./hardware-configuration.nix
    # "${builtins.fetchTarball "https://github.com/nix-community/disko/archive/master.tar.gz"}/module.nix"
    ./disk-config.nix
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
    reflector = true;
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

  age.secrets = {
    namecheap.file = ../../secrets/namecheap.age;
    jellyfin-admin.file = ../../secrets/jellyfin-admin.age;
    hass = {
      file = ../../secrets/hass.age;
      path = "${config.services.home-assistant.configDir}/secrets.yaml";
      owner = "hass";
      group = "hass";
    };

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
      #dnsPropagationCheck = false;
      dnsPropagationCheck = true;
      #webroot = "/var/lib/acme";
      environmentFile = config.age.secrets.namecheap.path;
    };
  };

  # only meaningful while jellyfin is enabled (see below)
  users.users.${config.services.jellyfin.user} =
    pkgs.lib.mkIf config.services.declarative-jellyfin.enable
      {
        extraGroups = [
          "video"
          "render"
        ];
      };

  # jellyfin disabled temporarily: jellyfin-init migration OOM-loops
  # (27.6G RSS during the DB migration run). Note: the full
  # services.declarative-jellyfin block lives here (jellyfin.nix is NOT
  # imported by default.nix — dead file, kept for reference).
  # Re-enable once root cause is sorted:
  # https://github.com/Sveske-Juice/declarative-jellyfin/issues/32
  services.declarative-jellyfin = {
    enable = false;
    group = "data";
    system = {
      serverName = "My Declarative Jellyfin Server";

      isStartupWizardCompleted = true;
      # use hardware acceleration for trickplay image generation
      trickplayOptions = {
        enableHwAcceleration = true;
        enableHwEncoding = true;
      };
      UICulture = "en";
    };
    libraries = {
      Movies = {
        enabled = true;
        contentType = "movies";
        pathInfos = [ "/data/Movies" ];
        typeOptions.Movies = {
          metadataFetchers = [
            "The Open Movie Database"
            "TheMovieDb"
          ];
          imageFetchers = [
            "The Open Movie Database"
            "TheMovieDb"
          ];
        };
      };
      Shows = {
        enabled = true;
        contentType = "tvshows";
        pathInfos = [ "/data/Shows" ];
      };

    };

    users = {
      yuanw = {
        mutable = false;
        hashedPasswordFile = config.age.secrets.jellyfin-admin.path;
        permissions = {
          isAdministrator = true;
        };
      };
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
      # A *path* literal is copied into the nix store at build time and
      # embedded in the initrd regardless of which host builds. The previous
      # absolute-string value never resolved: the deployed initrd shipped an
      # sshd with NO host key at all (sshd exits -> flap loop) = remote boot
      # was dead on arrival. Keep it out of git (see .gitignore) + copy it
      # to every build host (rsync) or eval fails loudly (path missing).
      hostKeys = [ ../../secrets/initrd/ssh_host_ed25519_key ];
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
  system.stateVersion = "25.05"; # Did you read the comment?

}
