# Home Manager side of browser-cli (config.toml + native messaging host).
# macOS: LibreWolf carries the extension (modules/browsers/browser-cli-darwin.nix).
# Firefox remains the daily browser when enabled.
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
  firefoxPkg = osConfig.modules.browsers.firefox.pkg or null;
  librewolfPath = "/Applications/Nix Casks/LibreWolf.app/Contents/MacOS/librewolf";
  browserPath =
    if pkgs.stdenv.isDarwin then
      librewolfPath
    else
      "${if firefoxPkg != null then firefoxPkg else pkgs.firefox}/bin/firefox";
in
{
  config = lib.mkIf enable {
    xdg.configFile."browser-cli/config.toml".text = ''
      firefox_path = "${browserPath}"
    '';

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
