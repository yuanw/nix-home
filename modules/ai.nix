{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.ai;
in
{
  options.modules.ai = {
    enable = mkEnableOption "ai";
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        # (pkgs.python3.withPackages (ps:
        #   with ps; [
        #     huggingface-hub
        #   ]))
        pkgs.ollama
      ];
    };
  };
}
