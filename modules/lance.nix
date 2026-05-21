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

  cfg = config.services.lance;

  # Convert extraConfig attrset to list of "KEY=value" strings
  extraEnv = lib.mapAttrsToList (name: value: "${name}=${value}") cfg.extraConfig;

  # Derive model directory name from variant
  modelDir = variant: if variant == "image" then "Lance_3B" else "Lance_3B_Video";

  # Build environment list for a given instance
  mkInstanceEnv =
    instance:
    [
      "LANCE_DATA_DIR=${cfg.dataDir}"
      "MODEL_PATH=${cfg.dataDir}/downloads/${modelDir instance.model}"
      "GRADIO_TASK=${instance.gradioTask}"
      "SERVER_PORT=${toString instance.gradioPort}"
      "SERVER_NAME=${instance.serverName}"
      "GPU_DEVICE=${instance.gpuDevice}"
      "QUEUE_SIZE=${toString instance.queueSize}"
      "PYTHONWARNINGS=ignore"
      "CUDA_LAUNCH_BLOCKING=0"
      "NCCL_DEBUG=VERSION"
      "POSITION_EMBEDDING_3D_VERSION=v2"
    ]
    ++ extraEnv;
in
{
  options.services.lance = {
    enable = mkEnableOption "Lance multimodal AI inference server";

    package = mkPackageOption pkgs "lance" { };

    dataDir = mkOption {
      type = types.path;
      default = "/var/lib/lance";
      description = ''
        Working directory for Lance.
        Model weights should be present here under downloads/.
        Use lance-download-model to download them.
      '';
    };

    openFirewall = mkOption {
      type = types.bool;
      default = true;
      description = "Open the firewall for Gradio server ports.";
    };

    extraConfig = mkOption {
      type = types.attrsOf types.str;
      default = { };
      description = "Additional environment variables passed to the Lance server.";
      example = {
        POSITION_EMBEDDING_3D_VERSION = "v2";
        NCCL_DEBUG = "VERSION";
      };
    };

    instances = mkOption {
      type = types.attrsOf (
        types.submodule {
          options = {
            enable = mkEnableOption "this Lance instance";
            model = mkOption {
              type = types.enum [
                "image"
                "video"
              ];
              description = ''
                Which Lance model variant this instance serves:
                - "image": Lance_3B (image tasks: t2i, image_edit, x2t_image)
                - "video": Lance_3B_Video (video tasks: t2v, video_edit, x2t_video)
              '';
            };
            gradioTask = mkOption {
              type = types.str;
              default = "t2v";
              description = "Default Gradio task for this instance.";
            };
            gradioPort = mkOption {
              type = types.port;
              default = 7860;
              description = "TCP port for the Gradio web UI.";
            };
            serverName = mkOption {
              type = types.str;
              default = "0.0.0.0";
              description = "Bind address for the Gradio server.";
            };
            gpuDevice = mkOption {
              type = types.str;
              default = "0";
              description = "GPU device index (comma-separated for multi-GPU).";
            };
            queueSize = mkOption {
              type = types.ints.positive;
              default = 32;
              description = "Maximum number of queued Gradio requests.";
            };
          };
        }
      );
      default = { };
      description = ''
        Lance server instances. Each instance runs a separate Gradio server
        with its own model variant, port, and task.
      '';
      example = {
        video = {
          enable = true;
          model = "video";
          gradioTask = "t2v";
          gradioPort = 7860;
        };
        image = {
          enable = false;
          model = "image";
          gradioTask = "t2i";
          gradioPort = 7861;
        };
      };
    };
  };

  config = mkIf cfg.enable {
    users.users.lance = {
      isSystemUser = true;
      group = "lance";
      home = cfg.dataDir;
      createHome = true;
      extraGroups = [ "video" ]; # GPU access
    };
    users.groups.lance = { };

    # Create a systemd service for each enabled instance
    systemd.services = lib.mapAttrs' (
      name: instance:
      lib.nameValuePair "lance-gradio-${name}" (
        lib.mkIf instance.enable {
          description = "Lance Multimodal AI Model – Gradio Server (${instance.model}, ${name})";
          after = [ "network-online.target" ];
          wants = [ "network-online.target" ];
          wantedBy = [ "multi-user.target" ];

          serviceConfig = {
            Type = "simple";
            User = "lance";
            Group = "lance";
            StateDirectory = "lance";
            WorkingDirectory = cfg.dataDir;
            ExecStart = "${cfg.package}/bin/lance-gradio";
            Restart = "on-failure";
            RestartSec = "10s";
            PrivateTmp = false;
            # Wait up to 10 minutes for model to load
            TimeoutStartSec = 600;
            # Prevent OOM kills on model reload
            OOMScoreAdjust = -500;
            # GPU device access
            # NOTE: DeviceAllow + User=lance causes torch.cuda.is_available() to return False
            # on DGX Spark (GB10 unified memory). NVIDIA devices are world-writable (0666)
            # so no explicit DeviceAllow is needed.
            # DeviceAllow = "/dev/nvidia*";
            # Memory limit (adjust based on VRAM)
            MemoryMax = "96G";

            Environment = mkInstanceEnv instance;
          };
        }
      )
    ) cfg.instances;

    # ─── System packages: add lance CLI + model downloader ────
    environment.systemPackages = [
      cfg.package
      pkgs.lance-download-model
    ];

    # ─── Firewall: open all instance ports ───────────────────────
    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall (
      lib.flatten (lib.mapAttrsToList (_: instance: [ instance.gradioPort ]) cfg.instances)
    );
  };
}
