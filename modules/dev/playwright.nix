{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.dev.playwright;
in
{
  options.modules.dev.playwright = {
    enable = mkEnableOption "playwright";

    package = mkOption {
      type = types.package;
      default = pkgs.playwright-driver;
      description = "Playwright package to use";
    };

    enableTest = mkOption {
      type = types.bool;
      default = true;
      description = "Include playwright-test package";
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [
        cfg.package
        cfg.package.browsers
      ]
      ++ (lib.optionals cfg.enableTest [ pkgs.playwright-test ]);

      home.sessionVariables = {
        PLAYWRIGHT_BROWSERS_PATH = "${cfg.package.browsers}";
        PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";
        PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS = "true";
      };
    };
  };
}
