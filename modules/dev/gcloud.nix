{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.dev.gcloud;
in
{
  options.modules.dev.gcloud = {
    enable = mkEnableOption "Google Cloud SDK (gcloud and bq CLIs)";
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username}.home.packages = [ pkgs.google-cloud-sdk ];
  };
}
