{ config, lib, pkgs, ... }:
with lib;
let cfg = config.modules.dev.haskell;
in {

  options.modules.dev.zig = { enable = mkEnableOption "zig"; };

  config = mkIf cfg.enable {

    home.packages = [ pkgs.zig pkgs.zls ];
  };
}
