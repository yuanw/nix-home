{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.dev.java;
in
{
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
        (pkgs.jdt-language-server.overrideAttrs (
          _finalAttrs: _previousAttrs: {
            postPatch = ''
              # We store the plugins, config, and features folder in different locations
              # than in the original package. In addition, hard-code the path to the jdk
              # in the wrapper, instead of searching for it in PATH at runtime.
              substituteInPlace bin/jdtls.py \
                --replace "jdtls_base_path = Path(__file__).parent.parent" "jdtls_base_path = Path(\"$out/share/java/jdtls/\")"
            '';
          }
        ))

      ];
      programs = {
        java = {
          package = cfg.pkg;
          enable = false;
        };
        # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/development/libraries/java/lombok/default.nix#L26
        zsh = {
          sessionVariables = {
            LOMBOK_DIR = "${pkgs.lombok}/share/java";
          };
        };
      };
    };
  };
}
