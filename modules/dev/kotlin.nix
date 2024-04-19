{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.kotlin;
in {
  options.modules.dev.kotlin = {
    enable = mkEnableOption "kotlin";

  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = with pkgs; [
        kotlin
        kotlin-language-server
        ktlint
      ];
    };
  };
}
