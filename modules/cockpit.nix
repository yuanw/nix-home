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
    types
    mkDefault
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
  };

  config = mkIf cfg.enable {
    # Enable nixpkgs' built-in Cockpit module
    services.cockpit = {
      enable = true;
      port = cfg.port;
      openFirewall = cfg.openFirewall;

      # Add cockpit plugins
      plugins = with pkgs; [
        cockpit-machines
        cockpit-podman
      ];
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
