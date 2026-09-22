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
      ]
      ++ (lib.optionals cfg.enableTest [ pkgs.playwright-test ]);

      # Do not pin PLAYWRIGHT_BROWSERS_PATH / SKIP_BROWSER_DOWNLOAD to the nix
      # store: long-lived shells keep a stale path after rebuild, and the store
      # is read-only so `playwright install` cannot self-heal. Projects use
      # ~/.cache/ms-playwright via `pnpm exec playwright install` instead.
      home.sessionVariables = {
        PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS = "true";
      };
    };
  };
}
