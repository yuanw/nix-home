{ config, lib, pkgs, localConfig, ... }:

with lib;
let cfg = config.programs.haskell;
in {
  options.programs.haskell = { enable = mkEnableOption "haskell"; };

  config = mkIf cfg.enable {
    home-manager.users.${localConfig.username} = {
      home.packages = [ pkgs.hls ];
    };
  };
}
