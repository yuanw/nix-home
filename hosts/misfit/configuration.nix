# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ pkgs, config, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    # ./hardware-configuration.nix
    # "${builtins.fetchTarball "https://github.com/nix-community/disko/archive/master.tar.gz"}/module.nix"
    ./disk-config.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  i18n.defaultLocale = "en_US.UTF-8";

  # networking.hostName = "nixos"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true; # Easiest to use and most distros use this by default.

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

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
    # nssmdns = true;
    openFirewall = true;
  };

  networking.firewall.allowedTCPPorts = [
    1400
    1900
  ];

  services.home-assistant = {
    enable = true;
    extraArgs = [ "--debug" ];
    extraPackages =
      python3Packages: with python3Packages; [
        isal
        gtts
        getmac

        aiohttp-fast-zlib
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
      "sonos"
      "spotify"
      "sun"
      "switchbot"
      "switchbot_cloud"
      "tasmota"
      "template"
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
  custom.services.isponsorblocktv.enable = true;

  age.secrets = {
    namecheap.file = ../../secrets/namecheap.age;
    isponsorblock-config.file = ../../secrets/isponsorblockvg.age;
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

  users.users.${config.services.jellyfin.user}.extraGroups = [
    "video"
    "render"
  ];

  services.declarative-jellyfin = {
    enable = true;
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
  users.groups.data = { };
  users.users.yuanw = {
    isNormalUser = true;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMSvr2qkdnG03/pGLo3aCFTnwmvojKO6m/W74ckC1RPW me@yuanwang.ca"
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

  # ethernet drivers to load: (run "lspci -v | grep -iA8 'network\|ethernet'")
  boot.initrd.availableKernelModules = [
    "igc"
    "r8169"
    "i40e"
  ];
  # boot.kernelParams = [ "ip=127.0.0.1::::lo:none" ];
  boot.kernelParams = [ "ip=::::nixos-initrd::dhcp" ];
  boot.initrd.network = {
    enable = true;
    ssh = {
      enable = true;
      port = 2222;
      hostKeys = [
        "/etc/secrets/initrd/ssh_host_ed_25519_key"
      ];
      authorizedKeys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMSvr2qkdnG03/pGLo3aCFTnwmvojKO6m/W74ckC1RPW me@yuanwang.ca"
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
    extraOptions = ''

      experimental-features = nix-command flakes

    '';
    settings = {
      auto-optimise-store = true;
      allowed-users = [
        "root"
        "yuanw"
      ];
      trusted-users = [
        "root"
        "yuanw"
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
