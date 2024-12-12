{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.typing;
  adept = pkgs.writeShellScriptBin "adept" ''
    case "$1" in
      "l1" | "level1" | "1" ) ttyper --language-file $XDG_CONFIG_HOME/ttyper/level1;;
      "l2" | "level2" | "2" ) ttyper --language-file $XDG_CONFIG_HOME/ttyper/level2;;
      "l3" | "level3" | "3" ) ttyper --language-file $XDG_CONFIG_HOME/ttyper/level3;;
      "l4" | "level4" | "4" ) ttyper --language-file $XDG_CONFIG_HOME/ttyper/level4;;
      "l5" | "level5" | "5" ) ttyper --language-file $XDG_CONFIG_HOME/ttyper/level5;;
      *) ttyper;;
    esac
  '';
in
{
  options.modules.typing = {
    enable = mkEnableOption "typing";
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        # pkgs.haskellPackages.gotta-go-fast
        adept
        pkgs.ttyper
      ];
      programs = {
        zsh = {
          sessionVariables = {
            TTYPER_CONFIG_DIR = "$XDG_CONFIG_HOME/ttyper";
          };
        };
      };
      xdg.configFile = {
        # "keymap.svg" = pkgs.reiryoku-firmware/share/reiryoku.svg;
        "ttyper/level1".source = ./level1.txt;
        "ttyper/level2".source = ./level2.txt;
        "ttyper/level3".source = ./level3.txt;
        "ttyper/level4".source = ./level4.txt;
        "ttyper/level5".source = ./level5.txt;
      };
    };
  };
}
