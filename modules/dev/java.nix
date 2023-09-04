{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.java;
in {
  options.modules.dev.java = { enable = mkEnableOption "java"; };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username}.home.packages = with pkgs; [
      lombok

      google-java-format
    ];
  };
}
