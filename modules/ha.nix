{
  config,
  lib,
  ...
}:
let
  cfg = config.custom.services.ha;
in
{
  options.custom.services.ha = {
    enable = lib.mkEnableOption "ha";
    dataDir = lib.mkOption {

      type = lib.types.str;

      default = "/var/lib/home-assistant";

    };
  };

  config = lib.mkIf cfg.enable {
    users.groups.ha = {

      gid = 200;

    };

    users.users.ha = {

      home = cfg.dataDir;

      createHome = true;

      isSystemUser = true;

      group = "ha";

      uid = 200;

    };
    virtualisation.oci-containers.containers.homeassistant = {
      image = "homeassistant/home-assistant:stable";
      autoStart = true;
      extraOptions = [
        "--pull=newer"
      ];
      volumes = [
        "${cfg.dataDir}:/config"
      ];
      ports = [
        "127.0.0.1:8123:8123"
        "127.0.0.1:8124:80"
      ];
      environment = {
        PUID = toString config.users.users.ha.uid;
        PGID = toString config.users.groups.ha.gid;
      };

    };
  };
}
