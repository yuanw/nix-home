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

  # Resolve the NVIDIA driver package for nvidia-smi.
  # cfg.nvidiaPackage is explicit override, or we try common
  # attrpaths.  The nvidia package may be a set (open/mod) so
  # fall back to the .mod variant.
  nvidiaPkgTry = config.hardware.nvidia.package or null;
  nvidiaDrv = cfg.nvidiaPackage or nvidiaPkgTry;
  nvidiaPkg =
    if lib.isDerivation nvidiaDrv then nvidiaDrv else (nvidiaDrv.mod or nvidiaDrv.open or null);
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

    certExtraDnsNames = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = ''
        Extra DNS names to add as SANs in the self-signed TLS certificate
        Cockpit uses when accessed directly (e.g. --port / :9090). They are
        merged with `localhost`, the hostname and `$hostname.local`. Add
        reverse-proxy hostnames here. Browsers will still flag the cert as
        self-signed, but no longer as a name mismatch.
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
      ]
      ++ optional cfg.enableGpu (pkgs.cockpit-gpu.override { nvidiaDriverPkg = nvidiaPkg; });
    };

    # Allow access from LAN hostnames (merges with nixpkgs default localhost origin).
    # The *.yuanw.me entry is the caddy reverse proxy on misfit; without it
    # cockpit rejects the proxied login/websocket.
    services.cockpit.allowed-origins = [
      "https://cockpit.yuanw.me"
      "https://${config.networking.hostName}.local:${toString cfg.port}"
      "http://${config.networking.hostName}.local:${toString cfg.port}"
      "https://${config.networking.hostName}:${toString cfg.port}"
      "http://${config.networking.hostName}:${toString cfg.port}"
    ];

    # Allow unencrypted HTTP on LAN and trust Caddy's forwarded headers so
    # cockpit emits wss:// in Content-Security-Policy (otherwise browsers block
    # wss://cockpit.yuanw.me/cockpit/socket after login).
    services.cockpit.settings.WebService = {
      AllowUnencrypted = mkDefault true;
      ProtocolHeader = mkDefault "X-Forwarded-Proto";
      ForwardedForHeader = mkDefault "X-Forwarded-For";
    };

    # cockpit-session needs cockpit-bridge in PATH (nixpkgs module no longer adds it)
    environment.systemPackages = [ pkgs.cockpit ];

    # Stable self-signed cert for direct :${cfg.port} access. cockpit-certificate-ensure
    # uses this filename when no cert exists with default SANs (localhost/127.0.0.1 only),
    # which makes browsers reject the direct URL as a name mismatch. Pre-create it once
    # with real SANs (hostname + .local + certExtraDnsNames); it persists across reboots.
    systemd.services."cockpit-cert" = {
      description = "Ensure persistent Cockpit TLS certificate";
      wantedBy = [ "cockpit.service" ];
      before = [ "cockpit.service" ];
      serviceConfig.Type = "oneshot";
      path = [
        pkgs.openssl
        pkgs.coreutils
      ];
      script =
        let
          dns = [
            "localhost"
            config.networking.hostName
            "${config.networking.hostName}.local"
          ]
          ++ cfg.certExtraDnsNames;
          san = lib.concatStringsSep "," (map (d: "DNS:${d}") dns ++ [ "IP:127.0.0.1" ]);
        in
        ''
          dir=/etc/cockpit/ws-certs.d
          mkdir -p "$dir"
          if [ ! -e "$dir/0-self-signed.cert" ]; then
            echo "generating persistent cockpit TLS certificate"
            openssl req -x509 -newkey rsa:3072 -nodes \
              -keyout "$dir/0-self-signed.key" \
              -out "$dir/0-self-signed.cert" \
              -days 3650 \
              -subj "/CN=${config.networking.hostName}" \
              -addext "subjectAltName=${san}" 2>/dev/null
            chmod 600 "$dir/0-self-signed.key"
            chmod 644 "$dir/0-self-signed.cert"
          fi
        '';
    };

    # Run the web service persistently instead of the per-connection socket churn that
    # wedged the session socket (silent drops after login) and caused the switch-time
    # "Dependency failed for Cockpit Web Service". Self-heal crashes. Order after the
    # DynamicUser providers that own the /run/cockpit runtime dirs at switch.
    systemd.services.cockpit = {
      wantedBy = [ "multi-user.target" ];
      after = [
        "cockpit-wsinstance-socket-user.service"
        "cockpit-session-socket-user.service"
      ];
      serviceConfig.Restart = "on-failure";
    };

    # cockpit-gpu manifest.json checks /usr/bin/nvidia-smi via path-exists
    # condition. Create the symlink via systemd tmpfiles since /usr/bin is
    # a real directory on NixOS (unlike /bin which is a symlink).
    # We use config.system.path which is the system profile derivation, and
    # reference nvidia-smi from there. This works because the nvidia driver
    # is always included in the system profile on dgx-spark.
    systemd.tmpfiles.rules = mkIf cfg.enableGpu [
      "L+ /usr/bin/nvidia-smi - - - - ${config.system.path}/bin/nvidia-smi"
    ];

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
