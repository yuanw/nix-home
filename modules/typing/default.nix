{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.typing;
  adept = pkgs.writeShellScriptBin "adept" ''
    case "$1" in
      "l1" | "level1" | "1" ) ttyper --language-file $XDG_CONFIG_HOME/ttyper/language/level1.txt;;
      "l2" | "level2" | "2" ) ttyper --language-file $XDG_CONFIG_HOME/ttyper/language/level2.txt;;
      "l3" | "level3" | "3" ) ttyper --language-file $XDG_CONFIG_HOME/ttyper/language/level3.txt;;
      "l4" | "level4" | "4" ) ttyper --language-file $XDG_CONFIG_HOME/ttyper/language/level4.txt;;
      "l5" | "level5" | "5" ) ttyper --language-file $XDG_CONFIG_HOME/ttyper/language/level5.txt;;
      *) ttyper;;
    esac
  '';
in {
  options.modules.typing = { enable = mkEnableOption "typing"; };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        # pkgs.haskellPackages.gotta-go-fast
        adept
        pkgs.ttyper
      ];
      programs = {
        zsh = {
          sessionVariables = { TTYPER_CONFIG_DIR = "$XDG_CONFIG_HOME/ttyper"; };
        };
      };

      xdg.configFile = {
        "ttyper/language/level1.txt".source = ./level1.txt;
        "ttyper/language/level2.txt".source = ./level2.txt;
        "ttyper/language/level3.txt".source = ./level3.txt;
        "ttyper/language/level4.txt".source = ./level4.txt;
        "ttyper/language/level5.txt".source = ./level5.txt;
      };
    };
  };
}
