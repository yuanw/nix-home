{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.programs.ledger;
in
{
  options.programs.ledger = {
    enable = mkEnableOption "ledger";
    enableZshIntegration = mkOption {
      default = true;
      type = types.bool;
      description = ''
        Whether to enable Zsh integration.
      '';
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = with pkgs; [
        ledger
      ];
      programs.zsh = mkIf cfg.enableZshIntegration {
        sessionVariables = {
          LEDGER_FILE = "";
        };
      };
    };
  };
}
