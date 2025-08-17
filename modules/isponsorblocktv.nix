{
  config,
  pkgs,
  lib,
  ...
}:

let
  inherit (lib)
    mkIf
    mkEnableOption
    mkOption
    mkPackageOption
    ;
  inherit (lib.types) str path;
  cfg = config.services.isponsorblocktv;
in
{
  options = {
    services.isponsorblocktv = {
      enable = mkEnableOption "SponsorBlock Server";

      package = mkPackageOption pkgs "isponsorblocktv" { };

      user = mkOption {
        type = str;
        default = "isponsorblocktv";
        description = "User account under which isponsorblocktv runs.";
      };

      group = mkOption {
        type = str;
        default = "isponsorblocktv";
        description = "Group under which isponsorblocktv runs.";
      };

      dataDir = mkOption {
        type = path;
        default = "/var/lib/isponsorblocktv";
        description = ''
          Base data directory,
          passed with `--datadir`
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    age.secrets = {
      isponsorblock-config = {
        file = ../secrets/isponsorblockvg.age;
        mode = "770";
        path = "/var/lib/isponsorblocktv/config.json";
        owner = "isponsorblocktv";
      };
    };
    systemd = {
      services.isponsorblocktv = {
        description = "isponsorblock Server";
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];
        wantedBy = [ "multi-user.target" ];

        serviceConfig = {
          # Type = "simple";
          User = "isponsorblocktv";
          # cfg.user;
          # Group = cfg.group;
          UMask = "0077";
          #WorkingDirectory = cfg.dataDir;
          ExecStart = " ${pkgs.isponsorblocktv}/bin/iSponsorBlockTV --data '${cfg.dataDir}'";
          Restart = "on-failure";
          TimeoutSec = 15;
          SuccessExitStatus = [
            "0"
            "143"
          ];

          # Security options:
          NoNewPrivileges = true;
          SystemCallArchitectures = "native";
        };
      };
    };

    users.users.isponsorblocktv = {
      group = "isponsorblocktv";
      isSystemUser = true;
    };

    users.groups.isponsorblocktv = { };

  };
}
