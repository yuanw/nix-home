{ inputs, config, lib, pkgs, ... }:

with pkgs.stdenv;
with lib; {

  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  system.stateVersion = 4;
  nix.maxJobs = 8;
  services.nix-daemon.enable = false;

  nixpkgs.overlays = [ inputs.emacs.overlay ];
  nixpkgs.config.allowUnfree = true;

  environment.shells = [ pkgs.zsh ];
  environment.systemPackages = [ pkgs.zsh pkgs.gcc ];
  programs.bash.enable = false;
  programs.zsh.enable = true;

  users.users.yuanwang.shell = pkgs.zsh;
  users.users.yuanwang.home = "/Users/yuanwang";

  fonts.enableFontDir = true;
  fonts.fonts = with pkgs; [
    emacs-all-the-icons-fonts
    fira-code
    font-awesome
    roboto
    roboto-mono
  ];

  # Recreate /run/current-system symlink after boot
  services.activate-system.enable = true;

}
