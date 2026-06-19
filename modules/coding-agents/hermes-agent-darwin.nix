# launchd agents for hermes-agent (nix-darwin only). Kept out of hermes-agent.nix
# because that module is in myModules.common and is also loaded on NixOS, where
# `launchd` is not a valid option.
{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.hermes-agent;
  inherit (lib) mkIf mkMerge;

  launchdEnv = {
    HOME = config.my.homeDirectory;
    HERMES_HOME = "${config.my.homeDirectory}/.hermes";
    HERMES_MANAGED = "true";
  }
  // cfg.environment;
in
{
  config = mkMerge [
    (mkIf (cfg.enable && cfg.enableGateway) {
      launchd.user.agents.hermes-gateway.serviceConfig = {
        Label = "com.nousresearch.hermes-agent-gateway";
        ProgramArguments = [
          "${cfg.package}/bin/hermes"
          "gateway"
          "run"
        ];
        KeepAlive = true;
        StandardOutPath = "${config.my.homeDirectory}/.hermes/gateway.log";
        StandardErrorPath = "${config.my.homeDirectory}/.hermes/gateway.err.log";
        EnvironmentVariables = launchdEnv;
        WorkingDirectory = "${config.my.homeDirectory}/.hermes";
      };
    })
    (mkIf (cfg.enable && cfg.enableDashboard) {
      launchd.user.agents.hermes-dashboard.serviceConfig = {
        Label = "com.nousresearch.hermes-agent-dashboard";
        ProgramArguments = [
          "${cfg.package}/bin/hermes"
          "dashboard"
          "--no-open"
          "--skip-build"
          "--host"
          cfg.dashboardHost
          "--port"
          (toString cfg.dashboardPort)
        ];
        KeepAlive = true;
        StandardOutPath = "${config.my.homeDirectory}/.hermes/dashboard.log";
        StandardErrorPath = "${config.my.homeDirectory}/.hermes/dashboard.err.log";
        EnvironmentVariables = launchdEnv;
        WorkingDirectory = "${config.my.homeDirectory}/.hermes";
      };
    })
  ];
}
