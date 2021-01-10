# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let sources = import ../../nix/sources.nix;
in
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    "${sources.home-manager}/nixos"
    ../../modules/common.nix
  ];

  # nixpkgs.config.allowUnfree= true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.loader.grub = {
    enable = true;
    version = 2;
    efiSupport = true;
    enableCryptodisk = true;
    device = "nodev";
    font =
      "${pkgs.pragmata-pro-font}/share/fonts/PragmataPro/PragmataPro_Mono_R_0828.ttf";
    fontSize = 48;
  };

  boot.initrd.luks.devices.home = {
    device = "/dev/disk/by-uuid/e99187a6-5069-4db9-ba90-09ca8233577b";
    preLVM = true;
  };
  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "asche"; # Define your hostname.
  networking.networkmanager.enable =
    true; # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/Regina";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  #networking.interfaces.wlp0s20f0u8u3.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Enable the GNOME 3 Desktop Environment.
  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;
  #services.xserver.windowManager.xmonad.enable = true;

  # Configure keymap in X11
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.yuanwang = {
    isNormalUser = true;
    home = "/home/yuanwang";
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  nix = {
    trustedUsers = [ "root" "yuanwang" ];
    allowedUsers = [ "root" "yuanwang" ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [ wget vim gnupg ];

  fonts.fonts = with pkgs; [ pragmata-pro-font ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.ssh.startAgent = false;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gnome3";
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
  programs.python.enable = true;
  programs.editors.emacs = {
    enable = true;
    enableDoomConfig = false;
    pkg = pkgs.emacsPgtkGcc;
  };
  home-manager.users.yuanwang.programs = {
    pet.enable = true;

    git = {
      userEmail = "me@yuanwang.ca";

      signing = {
        key = "BF2ADAA2A98F45E7";
        signByDefault = true;
      };

      extraConfig = { github.user = "yuanw"; };
    };
  };
}
