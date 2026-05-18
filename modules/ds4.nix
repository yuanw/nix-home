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
    types
    ;
  cfg = config.services.ds4;
in
{
  options.services.ds4 = {
    enable = mkEnableOption "DS4 DeepSeek V4 Flash inference server";

    package = mkPackageOption pkgs "ds4" { };

    host = mkOption {
      type = types.str;
      default = "0.0.0.0";
      description = "Bind address for the DS4 server.";
    };

    port = mkOption {
      type = types.port;
      default = 8000;
      description = "TCP port for the DS4 server.";
    };

    contextSize = mkOption {
      type = types.ints.positive;
      default = 100000;
      description = "Context size allocated at startup.";
    };

    dataDir = mkOption {
      type = types.path;
      default = "/var/lib/ds4";
      description = ''
        Working directory for the DS4 server.
        The model GGUF must be present here (e.g. via ds4-download-model).
      '';
    };

    openFirewall = mkOption {
      type = types.bool;
      default = true;
      description = "Open the firewall for the DS4 server port.";
    };
  };

  config = mkIf cfg.enable {
    users.users.ds4 = {
      isSystemUser = true;
      group = "ds4";
      home = cfg.dataDir;
      createHome = true;
      extraGroups = [ "video" ]; # GPU access
    };
    users.groups.ds4 = { };

    systemd.services.ds4-server = {
      description = "DS4 DeepSeek V4 Flash server";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        User = "ds4";
        Group = "ds4";
        WorkingDirectory = cfg.dataDir;
        ExecStart = "${cfg.package}/bin/ds4-server --host ${cfg.host} --port ${toString cfg.port} --ctx ${toString cfg.contextSize}";
        Restart = "on-failure";
        RestartSec = "5s";
        PrivateTmp = true; # isolate /tmp to avoid stale lock files
        Environment = "LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs.cudaPackages.cuda_cudart ]}";
      };
    };

    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [ cfg.port ];
  };
}
