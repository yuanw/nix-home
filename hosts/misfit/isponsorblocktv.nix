#https://github.com/JakeHillion/nixos/commit/4eaae0fa7538326a89a21dcbcec2efff065c3ed8
{
  config,
  pkgs,
  lib,
  ...
}:

let

  cfg = config.custom.services.isponsorblocktv;

  ver = "v2.2.1";

  ctl = pkgs.writeScriptBin "isponsorblocktv-config" ''


    #! ${pkgs.runtimeShell}


    set -e




    sudo systemctl stop podman-isponsorblocktv





    sudo ${pkgs.podman}/bin/podman run           \


        --rm -it                                 \


        --uidmap=0:${toString config.users.users.isponsorblocktv.uid}:1 \


        --gidmap=0:${toString config.users.groups.isponsorblocktv.gid}:1 \


        -v ${cfg.dataDir}:/app/data              \


        ghcr.io/dmunozv04/isponsorblocktv:${ver} \


        --setup-cli





    sudo systemctl start podman-isponsorblocktv


  '';

in

{

  options.custom.services.isponsorblocktv = {

    enable = lib.mkEnableOption "isponsorblocktv";

    dataDir = lib.mkOption {

      type = lib.types.str;

      default = "/var/lib/isponsorblocktv";

    };

  };

  config = lib.mkIf cfg.enable {

    environment.systemPackages = [ ctl ];

    users.groups.isponsorblocktv = {

      gid = 199;

    };

    users.users.isponsorblocktv = {

      home = cfg.dataDir;

      createHome = true;

      isSystemUser = true;

      group = "isponsorblocktv";

      uid = 199;

    };
    virtualisation.containers.enable = true;
    virtualisation.podman = {
      enable = true;

      # Create a `docker` alias for podman, to use it as a drop-in replacement
      dockerCompat = true;

      # Required for containers under podman-compose to be able to talk to each other.
      defaultNetwork.settings.dns_enabled = true;
    };
    virtualisation.oci-containers.containers.isponsorblocktv = {

      image = "ghcr.io/dmunozv04/isponsorblocktv:${ver}";

      extraOptions = [

        "--uidmap=0:${toString config.users.users.isponsorblocktv.uid}:1"

        "--gidmap=0:${toString config.users.groups.isponsorblocktv.gid}:1"

      ];

      volumes = [ "${cfg.dataDir}:/app/data" ];

    };

    systemd.tmpfiles.rules = [

      "d ${cfg.dataDir} 0700 isponsorblocktv isponsorblocktv - -"

    ];

  };

}
