{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.java;
in {
  options.modules.dev.java = { enable = mkEnableOption "java"; };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = with pkgs; [
        lombok
        google-java-format
      ];
      programs = {
        # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/development/libraries/java/lombok/default.nix#L26
        zsh = { sessionVariables = { LOMBOK_DIR = "${pkgs.lombok}/share/java"; }; };
      };
    };
  };
}
