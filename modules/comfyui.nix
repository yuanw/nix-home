{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.comfyui;
in
{
  options.services.comfyui = {
    enable = lib.mkEnableOption "ComfyUI diffusion model server";

    package = lib.mkOption {
      type = lib.types.package;
      default =
        pkgs.comfyui or (pkgs.writeShellScriptBin "comfyui" ''
          echo "ComfyUI not available — build it with: nix build --impure ..."
          exit 1
        '');
      description = "ComfyUI package to use.";
    };

    host = lib.mkOption {
      type = lib.types.str;
      default = "0.0.0.0";
      description = "Listen address.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8188;
      description = "Listen port.";
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Open firewall for the web UI.";
    };

    extraArgs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Extra CLI arguments.";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.comfyui = {
      description = "ComfyUI — diffusion model server";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "simple";
        ExecStart = "${lib.getExe cfg.package} --listen ${cfg.host} --port ${toString cfg.port} ${lib.escapeShellArgs cfg.extraArgs}";
        Restart = "on-failure";
        RestartSec = "10";
        User = "comfyui";
        Group = "comfyui";
        StateDirectory = "comfyui";
        WorkingDirectory = "/var/lib/comfyui";

        # GPU access
        DeviceAllow = [
          "char-nvidiactl"
          "char-nvidia-frontend"
          "char-nvidia-uvm"
        ];
        DevicePolicy = "closed";

        # Hardening
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        MemoryMax = "96G";
        TimeoutStartSec = 600;
      };

      environment = {
        HOME = "/var/lib/comfyui";
      };
    };

    users.users.comfyui = {
      isSystemUser = true;
      group = "comfyui";
      home = "/var/lib/comfyui";
      createHome = true;
    };
    users.groups.comfyui = { };

    networking.firewall = lib.mkIf cfg.openFirewall {
      allowedTCPPorts = [ cfg.port ];
    };

    # Add comfyui to system packages (CLI available for all users)
    environment.systemPackages = [ cfg.package ];
  };
}
