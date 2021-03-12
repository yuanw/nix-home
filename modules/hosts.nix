{ config, lib, pkgs, ... }:

with lib;
let cfg = config.programs.stevenBlackHosts;
in {
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
    environment.etc.hosts.source = "${pkgs.hosts}/share/hosts/hosts";
  };
}
