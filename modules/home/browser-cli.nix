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
  # Homebrew Firefox on macOS cannot install extensions from nix store paths (sandbox).
  # Deploy the XPI into the home directory and reference it via file:// policy.
  extensionPath = "${config.home.homeDirectory}/.browser-cli/browser-cli-extension.xpi";
  extensionInstallUrl = "file://${extensionPath}";
  browserCliPolicies = pkgs.callPackage ../../packages/browser-cli-policies.nix {
    inherit (micsSkills) browser-cli-extension;
    installUrl = extensionInstallUrl;
  };
  browserPath =
    if pkgs.stdenv.isDarwin then
      if firefoxEnabled then
        if firefoxPkg == null then
          "/Applications/Firefox.app/Contents/MacOS/firefox"
        else
          "${firefoxPkg}/Applications/Firefox.app/Contents/MacOS/firefox"
      else
        "/Applications/Nix Casks/LibreWolf.app/Contents/MacOS/librewolf"
    else
      "${if firefoxPkg != null then firefoxPkg else pkgs.firefox}/bin/firefox";
in
{
  config = lib.mkIf enable {
    home.file.".browser-cli/browser-cli-extension.xpi".source =
      "${micsSkills.browser-cli-extension}/browser-cli-extension.xpi";

    xdg.configFile."browser-cli/config.toml".text = ''
      firefox_path = "${browserPath}"
    '';

    programs.firefox.policies = lib.mkIf firefoxEnabled {
      ExtensionSettings = browserCliPolicies.ExtensionSettings;
    };

    home.activation.installBrowserCliHost = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ${micsSkills.browser-cli}/bin/browser-cli --install-host
    '';
  };
}
