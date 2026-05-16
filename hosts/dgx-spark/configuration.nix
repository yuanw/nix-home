# DGX Spark — NixOS configuration with impermanence via preservation
#
# Disk layout (see disk-config.nix):
#   /boot        — ESP partition (vfat)
#   /persist     — btrfs subvolume holding all persistent state
#   /            — tmpfs (ephemeral, recreated on every boot)
#
# Preservation (https://github.com/nix-community/preservation) manages
# bind-mounts and symlinks from the volatile root into /persist,
# replacing impermanence with a purely static (tmpfiles.d + mount units)
# approach. This requires systemd initrd.
#
# Reference: https://www.reddit.com/r/NixOS/comments/1ndo35n/

{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  # ─── Impermanence: volatile root ───────────────────────────────────
  #
  # Mount the root filesystem as tmpfs so the system starts from a clean
  # slate on every boot. All state that must survive reboots lives under
  # /persist and is wired in by the preservation module.
  fileSystems."/nix".neededForBoot = true;

  # ─── Preservation ──────────────────────────────────────────────────
  #
  # Requires systemd initrd — the module asserts this automatically.
  boot.initrd.systemd.enable = true;

  preservation = {
    enable = true;
    preserveAt."/persist" = {
      # ── System directories ────────────────────────────────────────
      directories = [
        "/etc/nixos"

        "var/lib/bluetooth"
        {
          directory = "/var/lib/nixos"; # NixOS user/group state
          inInitrd = true;
        }
        "/var/lib/systemd/coredump"
        "/var/lib/systemd/timers"
        "/var/log"
        "/var/lib/fwupd" # Firmware updates
        "/var/lib/podman" # Container storage (podman is enabled by dgx-spark module)
        "/var/cache" # General cache dir
      ];

      # ── System files ──────────────────────────────────────────────
      files = [
        # Machine identity — read in initrd so it's available very early
        {
          file = "/etc/machine-id";
          inInitrd = true;
          how = "symlink";
        }
        # SSH host keys — symlink so they persist across reboots but
        # live on the persistent volume directly
        {
          file = "/etc/ssh/ssh_host_ed25519_key";
          how = "symlink";
          configureParent = true;
        }
        {
          file = "/etc/ssh/ssh_host_ed25519_key.pub";
          how = "symlink";
          configureParent = true;
        }
        {
          file = "/etc/ssh/ssh_host_rsa_key";
          how = "symlink";
          configureParent = true;
        }
        {
          file = "/etc/ssh/ssh_host_rsa_key.pub";
          how = "symlink";
          configureParent = true;
        }
      ];

      # ── Per-user state ────────────────────────────────────────────
      users = {
        yuanw = {
          commonMountOptions = [
            "x-gvfs-hide"
            "x-gdu.hide"
          ];
          directories = [
            # Shell / desktop state
            ".config"
            ".ssh"
            {
              directory = ".cache";
              mode = "0750";
            }
            # Nix and dev tool state
            ".local/state/nix"
            ".local/share/direnv"
          ];
          files = [
            ".histfile"
          ];
        };
        root = {
          home = "/root";
          directories = [
            {
              directory = ".ssh";
              mode = "0700";
            }
          ];
        };
      };
    };
  };

  # ─── Bootloader ─────────────────────────────────────────────────────
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.configurationLimit = 10;

  # ─── DGX Spark hardware ─────────────────────────────────────────────
  hardware.dgx-spark.enable = true;

  # ─── Networking ─────────────────────────────────────────────────────
  networking.hostName = "dgx-spark";

  # Use systemd-networkd for predictable wired networking on a headless server.
  # NetworkManager is desktop-oriented; systemd-networkd is lighter and works
  # reliably without a user session.
  systemd.network.enable = true;
  networking.useNetworkd = true;
  # Wait for network to be online before starting services that need it
  # (SSH, podman, etc.).
  systemd.network.wait-online.enable = true;

  # DHCP on all wired interfaces — the DGX Spark has one or two Ethernet
  # ports. This matches eno*, enp*, eth* but ignores wlan/wwan.
  systemd.network.networks."10-wired" = {
    matchConfig.Name = "en* eth*";
    networkConfig = {
      DHCP = true;
      MulticastDNS = true;
    };
    dhcpV4Config.RouteMetric = 100;
    linkConfig.RequiredForOnline = "routable";
  };

  # ─── Time zone / locale ─────────────────────────────────────────────
  time.timeZone = "America/Regina";
  i18n.defaultLocale = "en_US.UTF-8";

  # ─── User accounts ──────────────────────────────────────────────────
  users.users.yuanw = {
    isNormalUser = true;
    shell = pkgs.zsh;
    # No password set — SSH key authentication only.
    # To set a password later: passwd yuanw
    # (or use initialPassword = "changeme" for first-boot convenience)
    hashedPassword = "$y$j9T$0"; # locked — SSH keys only
    extraGroups = [
      "wheel"
      "networkmanager"
      "video" # GPU access
      "docker" # Podman/docker compat
    ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMSvr2qkdnG03/pGLo3aCFTnwmvojKO6m/W74ckC1RPW me@yuanwang.ca"
    ];
  };

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

  # ─── SSH ───────────────────────────────────────────────────────────
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
    # Generate host keys on first boot if they don't exist in /persist yet.
    # This is needed because /etc/ssh is on tmpfs (impermanence) and the
    # symlink targets in /persist won't exist until keys are generated.
    hostKeys = [
      {
        path = "/persist/etc/ssh/ssh_host_ed25519_key";
        type = "ed25519";
      }
      {
        path = "/persist/etc/ssh/ssh_host_rsa_key";
        type = "rsa";
        bits = 4096;
      }
    ];
  };

  # ─── ZRAM swap ──────────────────────────────────────────────────────
  # Complementary to the disk swap partition for memory pressure handling
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
  ];

  # ─── Zsh ───────────────────────────────────────────────────────────
  programs.zsh.enable = true;

  # ─── Firmware updates ──────────────────────────────────────────────
  # Already enabled by dgx-spark module, but listed here for clarity
  services.fwupd.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data were taken. Do NOT change this after
  # the initial install.
  system.stateVersion = "25.11";
}
