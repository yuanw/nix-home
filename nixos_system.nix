{ config, lib, pkgs, ... }:

with pkgs.stdenv;
with lib; {
  networking.hostName = config.my.hostname;
  # # Use the systemd-boot EFI boot loader.
  # boot.loader.systemd-boot.enable = true;
  # boot.loader.efi.canTouchEfiVariables = true;
  # boot.loader.grub = {
  #   enable = true;
  #   version = 2;
  #   efiSupport = true;
  #   enableCryptodisk = true;
  #   device = "nodev";
  # };
  # boot.kernelPackages = pkgs.linuxPackages_latest;
  # # luks
  # boot.initrd.luks.devices.cryptboot = {
  #   preLVM = true;
  #   device = "/dev/disk/by-uuid/23aa6e7d-55ae-4f77-8994-bdbdcc8680a0";
  # };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  #networking.networkmanager = { enable = true; };
  #networking.interfaces.wlp0s20f0u4u4.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the GNOME 3 Desktop Environment.
  services.blueman.enable = true;
  services.xserver.enable = true;
  services.xserver.autorun = true;
  services.xserver.autoRepeatDelay = 200;
  services.xserver.autoRepeatInterval = 25;

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;

  services.picom.enable = true;
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };
  # # services.xserver.displayManager.gdm.enable = true;
  # services.xserver.desktopManager.gnome = { enable = true; };
  nix = {
    package = pkgs.nixFlakes;
    settings = {
      substituters = [
        "https://utdemir.cachix.org"
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "utdemir.cachix.org-1:mDgucWXufo3UuSymLuQumqOq1bNeclnnIEkD4fFMhsw="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      allowed-users = [ "root" config.my.username ];
      trusted-users = [ "root" config.my.username ];
      auto-optimise-store = true;
    };
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
    gc = { automatic = true; };
  };

  system.stateVersion = "21.11";
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = false;
    };

  };
  users.users.${config.my.username} = {
    isNormalUser = true;
    uid = 1000;
    home = config.my.homeDirectory;
    extraGroups = [ "audio" "jackaudio" "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {
    systemPackages = with pkgs; [ wget vim git firefox
                                  gnome.gnome-tweaks
                                ];


  shells = [ pkgs.zsh ];
  gnome.excludePackages = (with pkgs; [
  gnome-photos
  gnome-tour
]) ++ (with pkgs.gnome; [
  cheese # webcam tool
  gnome-music
  # gnome-terminal
  gedit # text editor
  epiphany # web browser
  geary # email reader
  # evince # document viewer
  gnome-characters
  totem # video player
  tali # poker game
  iagno # go game
  hitori # sudoku game
  atomix # puzzle game
]);  };
  programs.zsh.enable = true;
  programs.gnupg.agent.enable = true;
  time.timeZone = "America/Regina";

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
    #   pragmata-pro
  ];
}
