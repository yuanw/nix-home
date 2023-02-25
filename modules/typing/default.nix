{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.typing;
  adept = pkgs.writeShellScriptBin "adept" ''
    case "$1" in
      "l1" | "level1" | "1" ) ttyper --language-file level1;;
      "l2" | "level2" | "2" ) ttyper --language-file level2;;
      "l3" | "level3" | "3" ) gotta-go-fast -h 10 -prw 60 $XDG_CONFIG_HOME/colemak/level2.txt;;
      "l4" | "level4" | "4" ) gotta-go-fast -h 10 -prw 60 $XDG_CONFIG_HOME/colemak/level4.txt;;
      "l5" | "level5" | "5" ) gotta-go-fast -h 10 -prw 60 $XDG_CONFIG_HOME/colemak/level5.txt;;
      *) gotta-go-fast -w 60;;
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
        "ttyper/language/level6.txt".source = ./all-words.txt;
      };
    };
  };
}
