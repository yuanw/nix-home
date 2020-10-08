{ config, lib, pkgs, ... }:

with lib;
let
  homeDir = builtins.getEnv ("HOME");
  cfg = config.programs.dart;
in {
  options.programs.dart = { enable = mkEnableOption "dart"; };

  config = {
  };
}
