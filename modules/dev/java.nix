{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.java;
in {
  options.modules.dev.java = {
    enable = mkEnableOption "java";
    pkg = mkOption {
      type = types.package;
      default = pkgs.jdk17;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = with pkgs; [
        # (pkgs.gradle.override { java = cfg.pkg; })
        # maven
        lombok
        google-java-format
        # jdt-language-server
      ];
      programs = {
        java = {
          package = cfg.pkg;
          enable = true;
        };
        # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/development/libraries/java/lombok/default.nix#L26
        zsh = {
          sessionVariables = {
            LOMBOK_DIR = "${pkgs.lombok}/share/java";
            GRADLE_USER_HOME = "${config.my.homeDirectory}/.gradle";
          };
        };
      };
    };
  };
}
