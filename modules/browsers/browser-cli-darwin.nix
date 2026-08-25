# macOS: LibreWolf with browser-cli policy for automation.
# https://github.com/Mic92/dotfiles/blob/6040591/darwinModules/nix-casks.nix
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  micsSkills = inputs.mics-skills.packages.${pkgs.stdenv.hostPlatform.system};
  piEnabled = config.modules.pi.enable or false;
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;

  librewolfBrowserCliInstallUrl = "file://${micsSkills.browser-cli-extension}/browser-cli-extension.xpi";
  librewolfBrowserPath = "/Applications/Nix Casks/LibreWolf.app/Contents/MacOS/librewolf";

  browserCliPolicies = import ../../packages/browser-cli-policies.nix {
    inherit (micsSkills) browser-cli-extension;
    installUrl = librewolfBrowserCliInstallUrl;
  };

in
{
  config = lib.mkIf (piEnabled && isDarwin) {
    modules.pi.environment = {
      BROWSER_CLI_FIREFOX_PATH = librewolfBrowserPath;
    };

    modules.browsers.librewolf.darwinExtraPolicies = browserCliPolicies;
  };
}
