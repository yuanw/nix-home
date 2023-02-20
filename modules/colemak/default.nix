{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.colemak;
  colemak = pkgs.writeShellScriptBin "colemak" ''
    case "$1" in
      "l1" | "level1" | "1" ) gotta-go-fast -h 10 -prw 60 $XDG_CONFIG_HOME/colemak/level1.txt;;
      "l2" | "level2" | "2" ) gotta-go-fast -h 10 -prw 60 $XDG_CONFIG_HOME/colemak/level2.txt;;
      "l3" | "level3" | "3" ) gotta-go-fast -h 10 -prw 60 $XDG_CONFIG_HOME/colemak/level3.txt;;
      "l4" | "level4" | "4" ) gotta-go-fast -h 10 -prw 60 $XDG_CONFIG_HOME/colemak/level4.txt;;
      "l5" | "level5" | "5" ) gotta-go-fast -h 10 -prw 60 $XDG_CONFIG_HOME/colemak/level5.txt;;
      *) gotta-go-fast -w 60;;
    esac
  '';
in {
  options.modules.colemak = { enable = mkEnableOption "colemak"; };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        # colemak
        # pkgs.haskellPackages.gotta-go-fast
        ttyper
      ];

      xdg.configFile = {
        "colemak/level1.txt".source = ./level1.txt;
        "colemak/level2.txt".source = ./level2.txt;
        "colemak/level3.txt".source = ./level3.txt;
        "colemak/level4.txt".source = ./level4.txt;
        "colemak/level5.txt".source = ./level5.txt;
        "colemak/level6.txt".source = ./all-words.txt;
      };
    };
  };
}
