{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.java;
in {
  options.modules.dev.java = {
    enable = mkEnableOption "java";
    pkg = mkOption {
      type = types.package;
      default = pkgs.jdk21;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = with pkgs; [
        (maven.override
          { jdk = cfg.pkg; })
        lombok
        google-java-format
        jdtls
        (pkgs.writeShellScriptBin "jdtls-with-lombok"
          ''
            ${pkgs.jdtls}/bin/jdtls --jvm-arg=-javaagent:${pkgs.lombok}/share/java/lombok.jar
          '')
      ];
      programs = {
        java = {
          package = cfg.pkg;
          enable = true;
        };
        # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/development/libraries/java/lombok/default.nix#L26
        zsh = { sessionVariables = { LOMBOK_DIR = "${pkgs.lombok}/share/java"; }; };
      };
    };
  };
}
