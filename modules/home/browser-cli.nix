# Home Manager side of browser-cli (native messaging + config.toml + Firefox policies).
# macOS LibreWolf cask (when Firefox disabled): modules/browsers/browser-cli-darwin.nix
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
  firefoxEnabled = osConfig.modules.browsers.firefox.enable or false;
  firefoxPkg = osConfig.modules.browsers.firefox.pkg or null;
  nixCasksFirefox = "/Applications/Nix Casks/Firefox.app/Contents/MacOS/firefox";
  browserPath =
    if pkgs.stdenv.isDarwin then
      if firefoxEnabled then
        if firefoxPkg == null then
          nixCasksFirefox
        else
          "${firefoxPkg}/Applications/Firefox.app/Contents/MacOS/firefox"
      else
        "/Applications/Nix Casks/LibreWolf.app/Contents/MacOS/librewolf"
    else
      "${if firefoxPkg != null then firefoxPkg else pkgs.firefox}/bin/firefox";
in
{
  config = lib.mkIf enable {
    xdg.configFile."browser-cli/config.toml".text = ''
      firefox_path = "${browserPath}"
    '';

    home.activation.browserCliExtension = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      mkdir -p "$HOME/.browser-cli"
      cp -f ${micsSkills.browser-cli-extension}/browser-cli-extension.xpi "$HOME/.browser-cli/browser-cli-extension.xpi"
    '';

    home.activation.clearBrowserCliDefaultsPolicy = lib.hm.dag.entryAfter [ "writeBoundary" ] (
      lib.mkIf pkgs.stdenv.isDarwin ''
        # Baked into Firefox.app; stale defaults block extension install.
        /usr/bin/defaults delete org.mozilla.firefox ExtensionSettings 2>/dev/null || true
      ''
    );

    home.activation.installBrowserCliHost = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ${micsSkills.browser-cli}/bin/browser-cli --install-host
    '';
  };
}
