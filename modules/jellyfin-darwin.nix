{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.jellyfin;
in
{
  options.modules.jellyfin = {
    enable = mkEnableOption "jellyfin";
  };
  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        pkgs.jellyfin
        pkgs.qbittorrent
        pkgs.sonarr
        pkgs.lego
        #pkgs.caddy
        pkgs.nginx
      ];
    };
    launchd.user.agents.jellyfin = {
      path = [ pkgs.jellyfin ];
      serviceConfig = {
        ProgramArguments = [
          "${pkgs.lib.getExe pkgs.jellyfin}"
        ];
        StandardErrorPath = "/tmp/jellyfin.log";
        StandardOutPath = "/tmp/jellyfin.log";
        KeepAlive = true;
        RunAtLoad = true;
      };
    };
  };
}
