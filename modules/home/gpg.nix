{ pkgs, ... }:
let
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
in
{
  programs.gpg.enable = true;

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 600;
    maxCacheTtl = 7200;
    enableZshIntegration = true;
    pinentry.package = if isDarwin then pkgs.pinentry_mac else pkgs.pinentry-gnome3;
  };
}
