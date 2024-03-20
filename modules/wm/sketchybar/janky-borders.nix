{ config, lib, pkgs, ... }:

let
  inherit (lib) mdDoc mkEnableOption mkIf mkPackageOptionMD mkOption types;

  cfg = config.services.janky-borders;
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
          #!/bin/bash

          options=(
          style=round
          width=6.0
          hidpi=off
          active_color=0xc0e2e2e3
          inactive_color=0xc02c2e34
          background_color=0x302c2e34
          )
        '';
        description = mdDoc ''
          Contents of sketchybar's configuration file. If empty (the default), the configuration file won't be managed.

          See [documentation](https://felixkratz.github.io/SketchyBar/)
          and [example](https://github.com/FelixKratz/SketchyBar/blob/master/sketchybarrc).
        '';
      };
    };
  # https://github.com/FelixKratz/homebrew-formulae/blob/master/borders.rb#L35
  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    launchd.user.agents.borders = {
      path = [ cfg.package ] ++ [ config.environment.systemPath ];
      serviceConfig.ProgramArguments = [ "${cfg.package}/bin/borders" ];
      serviceConfig.KeepAlive = true;
      serviceConfig.RunAtLoad = true;
    };
  };
}
