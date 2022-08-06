{ config, lib, pkgs, ... }:

with pkgs.stdenv;
with lib; {
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
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  # hardware.pulseaudio.package = pkgs.pulseaudioFull;
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

  system.stateVersion = "22.05";
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
    systemPackages = with pkgs; [ wget vim git firefox ];
    shells = [ pkgs.zsh ];
  };
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
