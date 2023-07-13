# https://github.com/natsukium/dotfiles/blob/929ed6a27218f6312fe4bbd862bdaa60639cb544/nix/modules/services/sketchybar.nix
{ config
, lib
, pkgs
, ...
}:
with lib; let
  cfg = config.services.sketchybar;
  homeDir = config.my.homeDirectory;
in
{
  options.services.sketchybar.enable = mkEnableOption ''
    Whether to enable the sketchybar.
  '';

  options.services.sketchybar.package = mkOption {
    type = types.path;
    default = pkgs.sketchybar;
    description = "The sketcybar package to use.";
  };

  options.services.sketchybar.config = mkOption {
    type = types.str;
    default = "";
    example =
      literalExpression ''
      '';
    description = ''
      Configuration written to sketchybarrc.
    '';
  };

  options.services.sketchybar.hideMenuBar = mkOption {
    type = types.bool;
    default = true;
    description = "Whether to hide the default menu bar.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    launchd.user.agents.sketchybar = {
      serviceConfig = {
        StandardOutPath = "/tmp/sketchybar.out.log";
        StandardErrorPath = "/tmp/sketchybar.err.log";

        ProgramArguments =
          [ "${cfg.package}/bin/sketchybar" ];
        # ++ optionals (cfg.config != "") ["--config" configFile];
        KeepAlive = true;
        RunAtLoad = true;
        # only to pass gh and jq
        EnvironmentVariables = {
          PATH = "${cfg.package}/bin:${config.environment.systemPath}:${homeDir}/.nix-profile/bin";
        };
      };
    };

    system.defaults.NSGlobalDomain._HIHideMenuBar = cfg.hideMenuBar;
  };
}
