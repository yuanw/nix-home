{ config, lib, pkgs, ... }:

with lib;
let cfg = config.programs.node;
in {
  options.programs.node = { enable = mkEnableOption "node"; };

  config = mkIf cfg.enable {
    home-manager.users.yuanwang = {
      home.packages = [ pkgs.nodejs pkgs.yarn ]
        ++ (with pkgs.nodePackages; [ prettier ]);
    };
  };
}
