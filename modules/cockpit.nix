{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkIf
    mkEnableOption
    mkOption
    mkDefault
    optional
    types
    ;
  cfg = config.services.cockpit-local;
in
{
  options.services.cockpit-local = {
    enable = mkEnableOption "Cockpit web-based server manager";

    port = mkOption {
      type = types.port;
      default = 9090;
      description = "TCP port for Cockpit web UI.";
    };

    openFirewall = mkOption {
      type = types.bool;
      default = true;
      description = "Open firewall for Cockpit web UI.";
    };

    enableGpu = mkEnableOption "NVIDIA GPU monitoring plugin";

    nvidiaPackage = mkOption {
      type = types.nullOr types.package;
      default = null;
      description = ''
        Package providing nvidia-smi, injected into cockpit.service PATH.
        When unset, the module tries to auto-detect the driver package
        from common attrpaths (`hardware.nvidia.package` or
        `hardware.dgx-spark.package`). If auto-detection fails, set this
        explicitly.
      '';
    };
  };

  config = mkIf cfg.enable {
    # Enable nixpkgs' built-in Cockpit module
    services.cockpit = {
      enable = true;
      port = cfg.port;
      openFirewall = cfg.openFirewall;

      # Add cockpit plugins
      plugins = [
        pkgs.cockpit-machines
        pkgs.cockpit-podman
        pkgs.cockpit-metrics-nav
      ]
      ++ optional cfg.enableGpu (
        let
          # Resolve the NVIDIA driver package for nvidia-smi in PATH.
          # cfg.nvidiaPackage is explicit override, or we try common
          # attrpaths.  The nvidia package may be a set (open/mod) so
          # fall back to the .mod variant.
          nvidiaPkgTry = config.hardware.nvidia.package or null;
          nvidiaDrv = cfg.nvidiaPackage or nvidiaPkgTry;
          nvidiaPkg =
            if lib.isDerivation nvidiaDrv then nvidiaDrv else (nvidiaDrv.mod or nvidiaDrv.open or null);
        in
        pkgs.cockpit-gpu.override {
          nvidiaDriverPkg = nvidiaPkg;
        }
      );
    };

    # Allow access from LAN hostnames (merges with nixpkgs default localhost origin)
    services.cockpit.allowed-origins = [
      "https://${config.networking.hostName}.local:${toString cfg.port}"
      "http://${config.networking.hostName}.local:${toString cfg.port}"
      "https://${config.networking.hostName}:${toString cfg.port}"
      "http://${config.networking.hostName}:${toString cfg.port}"
    ];

    # Allow unencrypted HTTP on LAN (no self-signed cert warnings)
    services.cockpit.settings.WebService.AllowUnencrypted = mkDefault true;

    # cockpit-session needs cockpit-bridge in PATH (nixpkgs module no longer adds it)
    environment.systemPackages = [ pkgs.cockpit ];

    # Advertise via mDNS so spark.local:9090 is discoverable
    services.avahi = {
      publish.enable = true;
      extraServiceFiles = {
        cockpit = ''
          <?xml version="1.0" standalone='no'?>
          <!DOCTYPE service-group SYSTEM "avahi-service.dtd">
          <service-group>
            <name replace-wildcards="yes">DGX Spark Cockpit (%h)</name>
            <service>
              <type>_http._tcp</type>
              <port>${toString cfg.port}</port>
            </service>
          </service-group>
        '';
      };
    };
  };
}
