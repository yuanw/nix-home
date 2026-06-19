{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.hermes-agent;
  inherit (lib)
    mkEnableOption
    mkOption
    types
    mkIf
    mkMerge
    ;
  inherit (lib) literalExpression;
in
{
  options.modules.hermes-agent = {
    enable = mkEnableOption "hermes-agent";

    package = mkOption {
      type = types.package;
      default = pkgs.llm-agents.hermes-agent;
      description = "Which hermes-agent package to use.";
    };

    environment = mkOption {
      type = types.attrsOf types.str;
      default = { };
      example = literalExpression ''
        {
          DEEPSEEK_BASE_URL = "http://dgx-spark.local:8000/v1";
        }
      '';
      description = "Environment variables for hermes-agent (e.g. provider base URLs, API keys).";
    };

    config = mkOption {
      type = types.nullOr (types.attrsOf types.anything);
      default = null;
      example = literalExpression ''
        {
          model = "deepseek-v4-flash";
          custom_providers = [
            {
              name = "dgx-spark";
              base_url = "http://dgx-spark.local:8000/v1";
            }
          ];
        }
      '';
      description = ''
        Declarative hermes-agent config.yaml settings. Written to
        ~/.hermes/config.yaml at activation time using yq.
        Set to null to skip config management.
      '';
    };

    enableService = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Deprecated alias for enableGateway. Use enableGateway instead.
      '';
    };

    enableGateway = mkOption {
      type = types.bool;
      default = false;
      description = "Run hermes-agent gateway as a background service";
    };

    enableDashboard = mkOption {
      type = types.bool;
      default = false;
      description = "Run hermes-agent web dashboard as a background service";
    };

    dashboardHost = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Host address for hermes-agent web dashboard";
    };

    dashboardPort = mkOption {
      type = types.port;
      default = 9119;
      description = "Port for hermes-agent web dashboard";
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users.${config.my.username} = {
        home.packages = [ cfg.package ];

        home.sessionVariables = cfg.environment;

        home.activation.hermesDirs = ''
          mkdir -p "${config.my.homeDirectory}/.hermes/memories"
        '';

        home.activation.hermesEnv = lib.mkIf (cfg.environment != { }) ''
          cat > "${config.my.homeDirectory}/.hermes/.env" << 'HERMES_EOF'
          ${lib.concatStringsSep "\n" (lib.mapAttrsToList (n: v: "${n}=${v}") cfg.environment)}
          HERMES_EOF
          chmod 600 "${config.my.homeDirectory}/.hermes/.env"
        '';

        mergetools = mkIf (cfg.config != null) {
          "hermes-config" = {
            target = "${config.my.homeDirectory}/.hermes/config.yaml";
            format = "yaml";
            force = false;
            settings = cfg.config;
          };
        };
      };
    })
  ];
}
