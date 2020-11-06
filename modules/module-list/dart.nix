{ config, lib, pkgs, ... }:

with lib;
let
  homeDir = builtins.getEnv ("HOME");
  cfg = config.programs.dart;
in
{
  options.programs.dart = {
    enable = mkEnableOption "dart";
    package = mkOption {
      type = types.package;
      default = pkgs.dart;
      defaultText = literalExample "pkgs.dart";
      description = ''
        dart package to install.
      '';
    };

    enableZshIntegration = mkOption {
      default = true;
      type = types.bool;
      description = ''
        Whether to enable Zsh integration.
      '';
    };

  };

  config = mkIf cfg.enable {
    home-manager.users.yuanwang.home.packages = [ cfg.package ];
    home-manager.users.yuanwang.programs.zsh = mkIf cfg.enableZshIntegration {
      sessionVariables = { DART_SDK = "${cfg.package}"; };
      shellAliases = {
        ddev = "pub run dart_dev";
        pubcleanlock = ''
          git ls-files pubspec.lock --error-unmatch &>/dev/null && echo "Not removing pubspec.lock - it is tracked" || (rm pubspec.lock && echo "Removed pubspec.lock")'';
        pubclean = ''
          rm -r .pub .dart_tool/pub && echo "Removed .pub/"; find . -name packages | xargs rm -rf && echo "Removed packages/"; rm .packages && echo "Removed .packages"; pubcleanlock'';
        repub = "pubclean; pub get";
      };
      initExtra = ''
        export PATH=$PATH:$HOME/.local/bin:$HOME/.pub-cache/bin

        function dartUpgrade() {
              pub cache repair
              pub global activate dart_language_server
              pub global activate webdev_proxy
              pub global activate webdev
        }
      '';
    };
  };
}
