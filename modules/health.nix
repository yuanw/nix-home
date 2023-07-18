{ config, lib, pkgs, ... }:

with lib;
let
  homeDir = config.my.homeDirectory;
  cfg = config.modules.health;
in
{
  options.modules.health = { enable = mkEnableOption "health"; };

  config = mkIf cfg.enable {
    launchd.user.agents.stretchly = {
      # path = [ config.environment.systemPath ];

      serviceConfig = {
        StandardOutPath = "/tmp/strecthly.log";
        StandardErrorPath = "/tmp/strecthly.log";
        ProgramArguments =
          [
            "${pkgs.haskellPackages.mono-stretchly}/bin/mono-strecthly"
          ];
        RunAtLoad = false;
        EnvironmentVariables = {
          PATH = "${config.environment.systemPath}:${homeDir}/.nix-profile/bin";
        };

        StartCalendarInterval = [{ Minute = 15; }];
      };
      # serviceConfig.UserName = cfg.user;
    };
    home-manager.users.${config.my.username} = {
      home.packages = [
        pkgs.haskellPackages.mono-stretchly
        pkgs.SDL2
        pkgs.glew
      ];
    };
  };
}
