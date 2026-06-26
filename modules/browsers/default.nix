# Shared default-browser settings for macOS integrations (yabai, Emacs, duti).
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.browsers;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;

  appDisplayNames = {
    firefox = "Firefox";
    librewolf = "LibreWolf";
    chromium = "Chromium";
    safari = "Safari";
  };

  pkgFor =
    browser:
    if browser == "firefox" then
      config.modules.browsers.firefox.pkg or null
    else if browser == "librewolf" then
      config.modules.browsers.librewolf.pkg or null
    else if browser == "chromium" then
      config.modules.browsers.chromium.pkg or null
    else
      null;

  darwinAppName = appDisplayNames.${cfg.defaultBrowser};

  darwinOpenTarget =
    if !isDarwin then
      null
    else if cfg.defaultBrowser == "safari" then
      "Safari"
    else
      let
        pkg = pkgFor cfg.defaultBrowser;
      in
      if pkg == null then
        "/Applications/Nix Casks/${darwinAppName}.app"
      else
        "${config.my.homeDirectory}/Applications/Home Manager Apps/${darwinAppName}.app";

  darwinLaunchCmd = lib.mkIf isDarwin "open -n -a \"${darwinOpenTarget}\"";
in
{
  options.modules.browsers = {
    defaultBrowser = lib.mkOption {
      type = lib.types.enum [
        "firefox"
        "librewolf"
        "chromium"
        "safari"
      ];
      default = "firefox";
      description = ''
        Default GUI browser for macOS integrations: yabai launcher, Emacs
        browse-url, and `defaultbrowser` registration via gecko-home.
      '';
    };

    darwinAppName = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      readOnly = true;
      visible = false;
    };

    darwinOpenTarget = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      readOnly = true;
      visible = false;
    };

    darwinLaunchCmd = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      readOnly = true;
      visible = false;
    };

    defaultbrowserCliName = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      visible = false;
    };
  };

  config = {
    assertions = [
      {
        assertion =
          cfg.defaultBrowser == "safari" || (config.modules.browsers.${cfg.defaultBrowser}.enable or false);
        message = "modules.browsers.defaultBrowser is `${cfg.defaultBrowser}` but that browser is not enabled.";
      }
    ];

    modules.browsers = {
      inherit darwinAppName darwinOpenTarget darwinLaunchCmd;
      defaultbrowserCliName = cfg.defaultBrowser;
    };
  };
}
