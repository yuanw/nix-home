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
        pkgs.jellyfin-web

      ];
    };
    launchd.user.agents.jellyfin = {
      path = [ pkgs.jellyfin ];
      serviceConfig = {
        ProgramArguments = [
          "${pkgs.lib.getExe pkgs.jellyfin} -d $HOME/.local/share/jellyfin -c $HOME/.config/jellyfin -C $HOME/.cache/jellyfin"
        ];
        StandardErrorPath = "/tmp/jellyfin.log";
        StandardOutPath = "/tmp/jellyfin.log";
        KeepAlive = true;
        RunAtLoad = true;
      };
    };
  };
}
