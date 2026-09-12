{
  config,
  inputs',
  lib,
  options,
  ...
}:

with lib;
let
  cfg = config.modules.mouseless;
in
{

  options.modules.mouseless = {
    enable = mkEnableOption "mouseless";
  };
  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.modules.brew.enable;
        message = "need homebrew to install mouseless (for now)";
      }
      {
        assertion = !config.modules.neru.enable;
        message = "modules.mouseless and modules.neru cannot both be enabled on the same host";
      }

    ];

    environment.casks = mkIf (options ? environment.casks) (mkAfter [
      inputs'.nix-casks.packages.mouseless
    ]);

    environment.loginItems.items = mkIf (options ? environment.loginItems.items) (mkAfter [
      "/Applications/Nix Casks/Mouseless.app"
    ]);

    home-manager.users.${config.my.username} =
      hm@{
        pkgs,
        ...
      }:
      {
        home.activation.copyMouselessConfig = hm.config.lib.dag.entryAfter [ "writeBoundary" ] ''
          configDir="$HOME/Library/Containers/net.sonuscape.mouseless/Data/.mouseless/configs"
          ${pkgs.coreutils}/bin/mkdir -p "$configDir"
          ${pkgs.coreutils}/bin/cp -f "${./config.yaml}" "$configDir/config.yaml"
        '';
      };
  };
}
