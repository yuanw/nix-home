{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.adguardhome-with-user;

  args = concatStringsSep " " (
    [
      "--no-check-update"
      "--pidfile /run/AdGuardHome/AdGuardHome.pid"
      "--work-dir /var/lib/AdGuardHome/"
      "--config /var/lib/AdGuardHome/AdGuardHome.yaml"
    ]
    ++ cfg.extraArgs
  );

  configFile = pkgs.writeTextFile {
    name = "AdGuardHome.yaml";
    text = builtins.toJSON cfg.settings;
    checkPhase = "${pkgs.adguardhome}/bin/adguardhome -c $out --check-config";
  };

in
{

  imports =
    let
      cfgPath = [
        "services"
        "adguardhome-with-user"
      ];
    in
    [
      (mkRenamedOptionModuleWith {
        sinceRelease = 2211;
        from = cfgPath ++ [ "host" ];
        to = cfgPath ++ [
          "settings"
          "bind_host"
        ];
      })
      (mkRenamedOptionModuleWith {
        sinceRelease = 2211;
        from = cfgPath ++ [ "port" ];
        to = cfgPath ++ [
          "settings"
          "bind_port"
        ];
      })
    ];

  options.services.adguardhome-with-user = with types; {
    enable = mkEnableOption ("AdGuard Home network-wide ad blocker");
    user = mkOption {
      type = types.str;
      example = "nss-user";
      description = "The username to use when connecting to the database";
    };
    passwordFile = mkOption {
      type = types.path;
      example = "/run/secrets/mysql-auth-db-passwd";
      description = "The path to the file containing the password for the user";
    };
    openFirewall = mkOption {
      default = false;
      type = bool;
      description = ''
        Open ports in the firewall for the AdGuard Home web interface. Does not
        open the port needed to access the DNS resolver.
      '';
    };

    mutableSettings = mkOption {
      default = true;
      type = bool;
      description = ''
        Allow changes made on the AdGuard Home web interface to persist between
        service restarts.
      '';
    };

    settings = mkOption {
      default = null;
      type = nullOr (submodule {
        freeformType = (pkgs.formats.yaml { }).type;
        options = {
          schema_version = mkOption {
            default = pkgs.adguardhome.schema_version;
            defaultText = literalExpression "pkgs.adguardhome.schema_version";
            type = int;
            description = ''
              Schema version for the configuration.
              Defaults to the `schema_version` supplied by `pkgs.adguardhome`.
            '';
          };
          bind_host = mkOption {
            default = "0.0.0.0";
            type = str;
            description = ''
              Host address to bind HTTP server to.
            '';
          };
          bind_port = mkOption {
            default = 3000;
            type = port;
            description = ''
              Port to serve HTTP pages on.
            '';
          };
        };
      });
      description = ''
        AdGuard Home configuration. Refer to
        <https://github.com/AdguardTeam/AdGuardHome/wiki/Configuration#configuration-file>
        for details on supported values.

        ::: {.note}
        On start and if {option}`mutableSettings` is `true`,
        these options are merged into the configuration file on start, taking
        precedence over configuration changes made on the web interface.

        Set this to `null` (default) for a non-declarative configuration without any
        Nix-supplied values.
        Declarative configurations are supplied with a default `schema_version`, `bind_host`, and `bind_port`.
        :::
      '';
    };

    extraArgs = mkOption {
      default = [ ];
      type = listOf str;
      description = ''
        Extra command line parameters to be passed to the adguardhome binary.
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion =
          cfg.settings != null
          ->
            cfg.mutableSettings
            || (hasAttrByPath [ "dns" "bind_host" ] cfg.settings)
            || (hasAttrByPath [ "dns" "bind_hosts" ] cfg.settings);
        message = "AdGuard setting dns.bind_host or dns.bind_hosts needs to be configured for a minimal working configuration";
      }
      {
        assertion =
          cfg.settings != null -> cfg.mutableSettings || hasAttrByPath [ "dns" "bootstrap_dns" ] cfg.settings;
        message = "AdGuard setting dns.bootstrap_dns needs to be configured for a minimal working configuration";
      }
    ];

    systemd.services.adguardhome-with-user = {
      description = "AdGuard Home: Network-level blocker";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      unitConfig = {
        StartLimitIntervalSec = 5;
        StartLimitBurst = 10;
      };

      preStart = optionalString (cfg.settings != null) ''
        echo "setting up adguard..."
        user_conf="$(mktemp)"
        conf_merge="$(mktemp)"

        if    [ -e "$STATE_DIRECTORY/AdGuardHome.yaml" ] \
           && [ "${toString cfg.mutableSettings}" = "1" ]; then
          # Writing directly to AdGuardHome.yaml results in empty file
          echo "found existing AdGuardHome.yaml"
          ${pkgs.apacheHttpd}/bin/htpasswd -B -b -n ${cfg.user} $(cat ${cfg.passwordFile})  | ${pkgs.gawk}/bin/awk -F:  'NR==1{ printf "{\"users\": [{\"name\": \"%s\", \"password\": \"%s\" }]}",  $1, $2 }' >> "$user_conf"
          cp --force "$user_conf"  "$STATE_DIRECTORY/user.json"
          ${pkgs.yaml-merge}/bin/yaml-merge "$user_conf" "${configFile}" > "$conf_merge"
          cp --force "$conf_merge"  "$STATE_DIRECTORY/temp.yaml"
          ${pkgs.yaml-merge}/bin/yaml-merge "$STATE_DIRECTORY/AdGuardHome.yaml" "$conf_merge" > "$STATE_DIRECTORY/AdGuardHome.yaml.tmp"
          mv "$STATE_DIRECTORY/AdGuardHome.yaml.tmp" "$STATE_DIRECTORY/AdGuardHome.yaml"
        else
          ${pkgs.apacheHttpd}/bin/htpasswd -B -b -n ${cfg.user} $(cat ${cfg.passwordFile})  | ${pkgs.gawk}/bin/awk -F:  'NR==1{ printf "{\"users\": [{\"name\": \"%s\", \"password\": \"%s\" }]}",  $1, $2 }' >> "$user_conf"
          cp --force "$user_conf"  "$STATE_DIRECTORY/user.json"
          ${pkgs.yaml-merge}/bin/yaml-merge "$user_conf" "${configFile}" > "$conf_merge"
          cp --force "$conf_merge" "$STATE_DIRECTORY/AdGuardHome.yaml"
          chmod 600 "$STATE_DIRECTORY/AdGuardHome.yaml"
        fi
      '';

      serviceConfig = {
        User = "adguardhome";
        ExecStart = "${pkgs.adguardhome}/bin/adguardhome ${args}";
        AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
        Restart = "always";
        RestartSec = 10;
        RuntimeDirectory = "AdGuardHome";
        StateDirectory = "AdGuardHome";
      };
    };

    users.users.adguardhome = {
      description = "adguard home user";
      group = "adguard";
      isSystemUser = true;
    };
    users.groups.adguard = { };

    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [ cfg.settings.bind_port ];
  };
}
