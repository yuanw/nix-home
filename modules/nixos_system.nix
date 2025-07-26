{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:

with pkgs.stdenv;
with lib;
{
  imports = [
    inputs.agenix.nixosModules.age
    # inputs.home-manager.nixosModules.home-manager
  ];

  networking.hostName = config.my.hostname;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  services.xserver.enable = true;
  services.xserver.autorun = true;
  services.xserver.autoRepeatDelay = 200;
  services.xserver.autoRepeatInterval = 25;
  # enable jackd seems mess up headphone jack
  # services.jack.jackd.enable = true;
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;
  # hardware.pulseaudio.package = pkgs.pulseaudioFull;
  nix = {
    # auto-optimise-store = true;
    # package = pkgs.nixVersions.git;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 10d";
    };
    settings = {
      allowed-users = [
        "root"
        config.my.username
      ];
      trusted-users = [
        "root"
        config.my.username
      ];
    };
  };

  system.stateVersion = "25.05";
  users.users.${config.my.username} = {
    isNormalUser = true;
    uid = 1000;
    home = config.my.homeDirectory;
    extraGroups = [
      "audio"
      "jackaudio"
      "wheel"
      # "docker"
    ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {
    systemPackages = with pkgs; [
      wget
      vim
      git
      firefox
    ];
    shells = [ pkgs.zsh ];
  };
  programs.zsh.enable = true;
  programs.gnupg.agent.enable = true;
  time.timeZone = "America/Regina";
  virtualisation.docker.enable = false;
  #home-manager.backupFileExtension = "backup";
  #home-manager.useGlobalPkgs = true;
  #home-manager.useUserPackages = false;
  home-manager.users.${config.my.username} = import ./home.nix {
    inherit
      inputs
      pkgs
      lib
      config
      ;
  };

  fonts.fontDir.enable = true;
  fonts.packages = with pkgs; [
    fira-code
    font-awesome
    iosevka
    roboto
    roboto-mono
  ];
}
