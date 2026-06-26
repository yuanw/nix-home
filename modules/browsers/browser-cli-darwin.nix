# macOS: LibreWolf with browser-cli policy for automation; Firefox stays the daily browser.
# https://github.com/Mic92/dotfiles/blob/6040591/darwinModules/nix-casks.nix
{
  config,
  lib,
  pkgs,
  inputs,
  inputs',
  ...
}:
let
  micsSkills = inputs.mics-skills.packages.${pkgs.stdenv.hostPlatform.system};
  piEnabled = config.modules.pi.enable or false;
  firefoxEnabled = config.modules.browsers.firefox.enable or false;
  isDarwin = pkgs.stdenv.isDarwin;
  nixCasksFirefox = inputs'.nix-casks.packages.firefox or null;

  librewolfBrowserCliInstallUrl = "file:///Applications/Nix Casks/LibreWolf.app/Contents/Resources/distribution/browser-cli-extension.xpi";
  librewolfBrowserPath = "/Applications/Nix Casks/LibreWolf.app/Contents/MacOS/librewolf";

  browserCliPolicies = import ../../packages/browser-cli-policies.nix {
    inherit (micsSkills) browser-cli-extension;
    installUrl = librewolfBrowserCliInstallUrl;
  };

  searchPolicies = import ../../packages/gecko-search-policies.nix;

  librewolfWithBrowserCli = pkgs.librewolf-macos.override {
    policies = lib.recursiveUpdate searchPolicies browserCliPolicies;
    inherit (micsSkills) browser-cli-extension;
  };
in
{
  config = lib.mkIf (piEnabled && isDarwin) {
    modules.pi.environment = {
      BROWSER_CLI_FIREFOX_PATH = librewolfBrowserPath;
    };

    environment.casks = [
      librewolfWithBrowserCli
    ]
    ++ lib.optionals (firefoxEnabled && nixCasksFirefox != null) [ nixCasksFirefox ];
  };
}
