{ config, lib, inputs', ... }:

with lib;
let
  cfg = config.modules.health;
in
{
  options.modules.health = { enable = mkEnableOption "health"; };

  config = mkIf cfg.enable {
    launchd.user.agents.stretchly = {
      path = [ inputs'.mono-strecthly-darwin.packages.default ];
      serviceConfig = {
        StandardOutPath = "/tmp/strecthly.log";
        StandardErrorPath = "/tmp/strecthly.log";
        ProgramArguments =
          [
            "${inputs'.mono-stretchly-darwin.packages.default}/bin/mono-stretchly"
          ];
        RunAtLoad = true;
        # in secs
        StartInterval = 900;
      };
      # serviceConfig.UserName = config.my.username;
    };
    home-manager.users.${config.my.username} = {
      home.packages = [
        inputs'.mono-stretchly-darwin.packages.default
      ];
    };
  };
}
