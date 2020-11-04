{ config, lib, pkgs, ... }:

with lib;
let
  sources = import ../../nix/sources.nix;
  cfg = config.programs.stevenBlackHosts;
in
{
  options.programs.stevenBlackHosts = {
    enable = mkEnableOption "stevenBlackHosts";
    # possible values
    # https://github.com/StevenBlack/hosts/tree/master/alternates
    category = mkOption {
      type = types.str;
      default = "fakenews-gambling-porn";
    };
  };

  config = mkIf cfg.enable {
    environment.etc.hosts.enable = true;
    environment.etc.hosts.source = sources.hosts
      + "/alternates/${cfg.category}/hosts";
  };
}
