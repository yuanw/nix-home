{ config, lib, pkgs, ... }:
let
  homeDir = builtins.getEnv ("HOME");
  configDir = ../../conf.d;
  cfg = config.programs.editors.emacs;
in with lib; {
  options.programs.editors.emacs = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    pkg = mkOption {
      type = types.package;
      default = pkgs.emacsMacport;
    };

    enableDoomConfig = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.yuanwang.programs.emacs = {
      enable = true;
      package = cfg.pkg;
    };

    home-manager.users.yuanwang.home.file =
      mkIf cfg.enableDoomConfig { ".doom.d".source = configDir + "/doom"; };

    home-manager.users.yuanwang.programs.zsh = {
      initExtra = ''
        export PATH=$HOME/.emacs.d/bin
      '';
    };
  };
}
