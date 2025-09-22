{ config, lib, ... }:

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

    ];
    modules.brew = {
      casks = [
        "mouseless@preview"
      ];
    };
    home-manager.users.${config.my.username} =
      hm@{
        ...
      }:
      {
        home = {
          file."Library/Containers/net.sonuscape.mouseless/Data/.mouseless/configs/config_base.yaml"

          .source =
            hm.config.lib.file.mkOutOfStoreSymlink "${config.my.homeDirectory}/${config.my.workspaceDirectory}/nix-home/modules/mouseless/config.yaml";

        };
      };
  };
}
