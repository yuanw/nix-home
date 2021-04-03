{ config, lib, pkgs, ... }:

with pkgs.stdenv;
with lib; {
  imports = [ ./hardware-configuration.nix ./modules ];
  networking.hostName = config.my.hostname;
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub = {
    enable = true;
    version = 2;
    efiSupport = true;
    enableCryptodisk = true;
    device = "nodev";
  };
  boot.kernelPackages = pkgs.linuxPackages_latest;
  # luks
  boot.initrd.luks.devices.cryptboot = {
    preLVM = true;
    device = "/dev/disk/by-uuid/23aa6e7d-55ae-4f77-8994-bdbdcc8680a0";
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp0s20f0u4u4.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the GNOME 3 Desktop Environment.
  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome3 = {
    enable = true;
    flashback.enableMetacity = true;
  };

  nix = {
    package = pkgs.nixFlakes;
    binaryCaches = [
      "https://utdemir.cachix.org"
      "https://hs-nix-template.cachix.org"
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
    ];
    binaryCachePublicKeys = [
      "utdemir.cachix.org-1:mDgucWXufo3UuSymLuQumqOq1bNeclnnIEkD4fFMhsw="
      "hs-nix-template.cachix.org-1:/YbjZCrYAw7d9ayLayk7ZhBdTEkR10ZFmFuOq6ZJo4c="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    trustedUsers = [ "root" config.my.username ];
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc = { automatic = true; };
  };

  system.stateVersion = "20.09";
  nixpkgs = {

    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = false;
    };

  };
  users.users.yuanwang = {
    isNormalUser = true;
    uid = 1000;
    home = "/home/yuanwang";
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [ wget vim git firefox ];
  environment.shells = [ pkgs.zsh ];
  programs.zsh.enable = true;
  programs.gnupg.agent.enable = true;
  time.timeZone = "America/Regina";

  users.users.yuanwang.shell = pkgs.zsh;
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = false;
  home-manager.users.${config.my.username} =
    import ./home.nix { inherit pkgs lib config; };

  fonts.fontDir.enable = true;
  fonts.fonts = with pkgs; [
    fira-code
    font-awesome
    iosevka
    roboto
    roboto-mono
    #pragmata-pro
  ];
  # Recreate /run/current-system symlink after boot
}
