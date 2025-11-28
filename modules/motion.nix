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
  inherit (lib.types)
    str
    path
    port
    int
    ;
  cfg = config.services.motion-camera;

  # Generate motion.conf dynamically
  motionConfigFile = pkgs.writeText "motion.conf" ''
    # Daemon
    daemon off
    process_id_file ${cfg.dataDir}/motion.pid

    # Capture
    videodevice /dev/video0
    width 1280
    height 720
    framerate 15

    # Output
    target_dir ${cfg.dataDir}/recordings
    snapshot_filename %Y%m%d-%H%M%S-snapshot
    picture_filename %Y%m%d-%H%M%S-%q
    movie_filename %Y%m%d-%H%M%S

    # Recording mode
    picture_output off
    movie_output on
    movie_max_time 3600
    movie_codec mkv

    # Motion detection
    threshold 1500
    minimum_motion_frames 1
    event_gap 60
    pre_capture 3
    post_capture 3

    # HTTP streaming
    stream_port ${toString cfg.streamPort}
    stream_localhost off
    stream_maxrate 5

    # Web control interface
    webcontrol_port ${toString cfg.port}
    webcontrol_localhost on
    webcontrol_authentication username:password

    # Logging
    log_level 6
    log_file ${cfg.dataDir}/motion.log
    log_type all
  '';
in
{
  options = {
    services.motion-camera = {
      enable = mkEnableOption "Motion camera system";

      package = mkPackageOption pkgs "motion" { };

      user = mkOption {
        type = str;
        default = "motion";
        description = "User account under which motion runs.";
      };

      group = mkOption {
        type = str;
        default = "motion";
        description = "Group under which motion runs.";
      };

      dataDir = mkOption {
        type = path;
        default = "/data/Cameras";
        description = ''
          Base directory for camera recordings.
        '';
      };

      port = mkOption {
        type = port;
        default = 8080;
        description = "HTTP port for motion web interface.";
      };

      streamPort = mkOption {
        type = port;
        default = 8081;
        description = "Port for camera stream.";
      };

      retentionDays = mkOption {
        type = int;
        default = 7;
        description = "Number of days to retain recordings.";
      };
    };
  };

  config = mkIf cfg.enable {
    # Systemd service for motion daemon
    systemd.services.motion = {
      description = "Motion Camera Detection Daemon";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.group;
        ExecStart = "${cfg.package}/bin/motion -n -c ${motionConfigFile}";
        Restart = "on-failure";
        RestartSec = 10;

        # Security hardening
        NoNewPrivileges = true;
        SystemCallArchitectures = "native";
        PrivateTmp = true;

        # Camera access
        SupplementaryGroups = [ "video" ];
      };
    };

    # Systemd service for cleanup
    systemd.services.motion-cleanup = {
      description = "Clean up old Motion recordings";
      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        ExecStart = "${pkgs.findutils}/bin/find ${cfg.dataDir}/recordings -type f -mtime +${toString cfg.retentionDays} -delete";
      };
    };

    # Systemd timer for cleanup
    systemd.timers.motion-cleanup = {
      description = "Timer for Motion recording cleanup";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "daily";
        Persistent = true;
      };
    };

    # Create directory structure
    systemd.tmpfiles.rules = [
      "d ${cfg.dataDir} 0750 ${cfg.user} ${cfg.group} -"
      "d ${cfg.dataDir}/recordings 0750 ${cfg.user} ${cfg.group} -"
      "d ${cfg.dataDir}/snapshots 0750 ${cfg.user} ${cfg.group} -"
    ];

    # Create motion user and group
    users.users.motion = {
      group = cfg.group;
      isSystemUser = true;
      extraGroups = [ "video" ]; # Access to /dev/video*
    };

    users.groups.motion = { };

    # Add motion user to data group for /data/ access
    users.groups.data.members = [ cfg.user ];
  };
}
