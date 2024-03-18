{ config, lib, pkgs, ... }:

let
  inherit (lib) literalExpression maintainers mdDoc mkEnableOption mkIf mkPackageOptionMD mkOption optionals types;

  cfg = config.services.janky-borders;

  configFile = pkgs.writeScript "bordersrc" cfg.config;
in

{

  options.services.sketchybar =
    {
      enable = mkEnableOption (mdDoc "jankyBorders");

      package = mkPackageOptionMD pkgs "jankyBorders" { };


      config = mkOption {
        type = types.lines;
        default = "";
        example = ''
          sketchybar --bar height=24
          sketchybar --update
          echo "sketchybar configuration loaded.."
        '';
        description = mdDoc ''
          Contents of sketchybar's configuration file. If empty (the default), the configuration file won't be managed.

          See [documentation](https://felixkratz.github.io/SketchyBar/)
          and [example](https://github.com/FelixKratz/SketchyBar/blob/master/sketchybarrc).
        '';
      };
    }
      # https://github.com/FelixKratz/homebrew-formulae/blob/master/borders.rb#L35
      config = mkIf cfg.enable {
  environment.systemPackages = [ cfg.package ];

  launchd.user.agents.sketchybar = {
    path = [ cfg.package ] ++ [ config.environment.systemPath ];
    serviceConfig.ProgramArguments = [ "${cfg.package}/bin/sketchybar" ]
      ++ optionals (cfg.config != "") [ "--config" "${configFile}" ];
    serviceConfig.KeepAlive = true;
    serviceConfig.RunAtLoad = true;
  };
};
}
