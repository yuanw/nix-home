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
        jdt-language-server
        # (pkgs.writeShellScriptBin "jdtls-with-lombok"
        #   ''
        #     ${pkgs.jdt-language-server}/bin/jdtls --jvm-arg=-javaagent:${pkgs.lombok}/share/java/lombok.jar
        #   '')
      ];
      programs = {
        java = {
          package = cfg.pkg;
          enable = false;
        };
        # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/development/libraries/java/lombok/default.nix#L26
        zsh = { sessionVariables = { LOMBOK_DIR = "${pkgs.lombok}/share/java"; }; };
      };
    };
  };
}
