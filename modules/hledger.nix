{ config, lib, pkgs, ... }:

with lib;
let cfg = config.programs.hledger;
in {
  options.programs.hledger = {
    enable = mkEnableOption "hledger";
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
      home.packages = with pkgs; [ hledge hledge-ui hledger-web ];
      programs.zsh = mkIf cfg.enableZshIntegration {
        sessionVariables = { LEDGER_FILE = ""; };
      };
    };
  };
}
