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
        mode = "0640";
        path = "${cfg.dataDir}/config.json";
        owner = "isponsorblocktv";
        group = "isponsorblocktv";
      };
    };
    systemd = {
      tmpfiles.rules = [
        "d '${cfg.dataDir}' 0750 isponsorblocktv isponsorblocktv - -"
      ];
      services.isponsorblocktv = {
        description = "isponsorblock Server";
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];
        wantedBy = [ "multi-user.target" ];

        serviceConfig = {
          # Type = "simple";
          User = "isponsorblocktv";
          # cfg.user;
          Group = "isponsorblocktv";
          UMask = "0077";
          #WorkingDirectory = cfg.dataDir;
          ExecStart = " ${pkgs.isponsorblocktv}/bin/iSponsorBlockTV --data '${cfg.dataDir}'";
          # agenix chowns the decrypted secret during system activation, which can
          # race with user creation on fresh installs — enforce ownership here so
          # the service can always read its config
          ExecStartPre = [
            "+${pkgs.coreutils}/bin/chown isponsorblocktv:isponsorblocktv '${cfg.dataDir}'"
            "+${pkgs.coreutils}/bin/chown isponsorblocktv:isponsorblocktv '${cfg.dataDir}/config.json'"
          ];
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
