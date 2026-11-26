# launchd agent for meridian (Antigravity backend), darwin only. Kept out of
# the systemd path because the meridian home module renders only a systemd
# user unit; on darwin the same package is run via a user launchd agent with
# the same env, built directly instead of parsing the unit's JSON env.
{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.meridian;
  inherit (lib) mkIf;
  home = config.my.homeDirectory;
  meridian = pkgs.callPackage "${inputs.meridian}/nix/pkgs/meridian/package.nix" {
    self = inputs.meridian;
  };
in
{
  options.modules.meridian = {
    enable = lib.mkEnableOption "meridian (Antigravity backend) as a launchd user agent";

    port = lib.mkOption {
      type = lib.types.port;
      default = 3457;
      description = "Loopback port meridian listens on.";
    };

    statePath = lib.mkOption {
      type = lib.types.path;
      default = "${home}/.local/state/meridian/antigravity.sqlite";
      description = "SQLite path for meridian's saved Antigravity state.";
    };
  };

  config = mkIf cfg.enable {
    launchd.user.agents.meridian.serviceConfig = {
      Label = "com.yuan.meridian";
      ProgramArguments = [ "${meridian}/bin/meridian" ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardOutPath = "${home}/Library/Logs/meridian.log";
      StandardErrorPath = "${home}/Library/Logs/meridian.log";
      EnvironmentVariables = {
        MERIDIAN_BACKEND = "antigravity";
        MERIDIAN_PORT = toString cfg.port;
        MERIDIAN_AGY_ALLOW_TOOL_BRIDGE = "1";
        MERIDIAN_AGY_STATE_PATH = cfg.statePath;
        MERIDIAN_AGY_PATH = "${config.modules.antigravity-cli.package}/bin/agy";
      };
    };
  };
}
