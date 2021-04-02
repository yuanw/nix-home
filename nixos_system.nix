{ inputs, config, lib, pkgs, ... }:

with pkgs.stdenv;
with lib; {
  imports = [ ./modules ];
  networking.hostName = config.my.hostname;
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
    gc = {
      automatic = true;
      user = "yuanwang";
    };
  };

  system.stateVersion = 4;
  nix.maxJobs = 8;
  services.nix-daemon.enable = false;
  nixpkgs = {
    overlays = [ inputs.nur.overlay inputs.emacs.overlay (import ./overlays) ];

    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = false;
    };

  };

  environment.shells = [ pkgs.zsh ];
  environment.systemPackages = [ pkgs.zsh pkgs.gcc ];
  programs.bash.enable = false;
  programs.zsh.enable = true;
  time.timeZone = "America/Regina";

  users.nix.configureBuildUsers = true;
  users.users.yuanwang.shell = pkgs.zsh;
  users.users.yuanwang.home = "/home/yuanwang";
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = false;
  home-manager.users.${config.my.username} =
    import ./home.nix { inherit pkgs lib config; };

  fonts.enableFontDir = true;
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
