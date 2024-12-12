{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.dev.node;
in
{
  options.modules.dev.node = {
    enable = mkEnableOption "node";
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        pkgs.nodejs
        pkgs.yarn
      ] ++ (with pkgs.nodePackages; [ prettier ]);
    };
  };
}
