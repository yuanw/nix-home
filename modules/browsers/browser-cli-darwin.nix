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
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  hmCfg = config.home-manager.users.${config.my.username} or { };
  micsSkillsCfg = hmCfg.programs.mics-skills or { };
  browserCliEnabled =
    (micsSkillsCfg.enable or false) && lib.elem "browser-cli" (micsSkillsCfg.skills or [ ]);

  librewolfBrowserCliInstallUrl = "file://${micsSkills.browser-cli-extension}/browser-cli-extension.xpi";
  librewolfBrowserPath =
    config.modules.browsers.librewolf.darwinBrowserExe
      or "${config.my.homeDirectory}/Applications/Home Manager Apps/LibreWolf.app/Contents/MacOS/librewolf";

  browserCliPolicies = import ../../packages/browser-cli-policies.nix {
    inherit (micsSkills) browser-cli-extension;
    installUrl = librewolfBrowserCliInstallUrl;
  };

in
{
  config = lib.mkIf (browserCliEnabled && isDarwin) {
    modules.pi.environment = {
      BROWSER_CLI_FIREFOX_PATH = librewolfBrowserPath;
    };

    modules.browsers.librewolf.darwinExtraPolicies = browserCliPolicies;
  };
}
