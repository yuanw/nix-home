{ config, lib, pkgs, ... }:
with lib;
let cfg = config.modules.dev.scheme;
in {

  options.modules.dev.scheme = { enable = mkEnableOption "scheme"; };
  # https://wingolog.org/archives/2013/01/07/an-opinionated-guide-to-scheme-implementations
  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      # http://wiki.call-cc.org/man/5/Using%20the%20interpreter
      home.packages = [ pkgs.chicken pkgs.guile ];
    };
  };
}
