# Home Manager side of browser-cli (config.toml + native messaging host).
# macOS: LibreWolf carries the extension (modules/browsers/browser-cli-darwin.nix).
# LibreWolf HM config mirrors Firefox via modules/browsers/gecko-home.nix.
# https://github.com/Mic92/mics-skills/tree/main/browser-cli#configuring-the-browser-path
{
  config,
  lib,
  pkgs,
  inputs,
  osConfig,
  ...
}:
let
  cfg = config.programs.mics-skills;
  enable = cfg.enable or false && lib.elem "browser-cli" (cfg.skills or [ ]);
  micsSkills = inputs.mics-skills.packages.${pkgs.stdenv.hostPlatform.system};

  librewolfCfg = osConfig.modules.browsers.librewolf or { enable = false; };
  firefoxPkg = osConfig.modules.browsers.firefox.pkg or null;
  librewolfPkg = librewolfCfg.pkg or pkgs.librewolf;

  # browser-cli uses firefox_path / BROWSER_CLI_FIREFOX_PATH for LibreWolf too.
  browserPath =
    if librewolfCfg.enable or false && pkgs.stdenv.isDarwin then
      "/Applications/Nix Casks/LibreWolf.app/Contents/MacOS/librewolf"
    else if librewolfCfg.enable or false then
      lib.getExe librewolfPkg
    else
      lib.getExe (if firefoxPkg != null then firefoxPkg else pkgs.firefox);
in
{
  config = lib.mkIf enable {
    xdg.configFile."browser-cli/config.toml".text = ''
      firefox_path = "${browserPath}"
    '';

    home.sessionVariables.BROWSER_CLI_FIREFOX_PATH = browserPath;

    home.activation.clearBrowserCliDefaultsPolicy = lib.hm.dag.entryAfter [ "writeBoundary" ] (
      lib.mkIf pkgs.stdenv.isDarwin ''
        /usr/bin/defaults delete org.mozilla.firefox ExtensionSettings 2>/dev/null || true
      ''
    );

    home.activation.installBrowserCliHost = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ${micsSkills.browser-cli}/bin/browser-cli --install-host
    '';
  };
}
