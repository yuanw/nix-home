{ config, lib, pkgs, ... }:

with lib;
let cfg = config.programs.haskell;
in {
  options.programs.haskell = { enable = mkEnableOption "haskell"; };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ pkgs.hls pkgs.ihp-new ];
    };
  };
}
