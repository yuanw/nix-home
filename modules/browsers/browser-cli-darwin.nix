# macOS: bake browser-cli extension policy into the browser .app (Nix Casks).
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

  # file:// into /nix/store is blocked by the macOS sandbox; ship the XPI inside the .app.
  firefoxBrowserCliInstallUrl = "file:///Applications/Nix Casks/Firefox.app/Contents/Resources/distribution/browser-cli-extension.xpi";
  librewolfBrowserCliInstallUrl = "file:///Applications/Nix Casks/LibreWolf.app/Contents/Resources/distribution/browser-cli-extension.xpi";

  browserCliPoliciesFor =
    installUrl:
    import ../../packages/browser-cli-policies.nix {
      inherit (micsSkills) browser-cli-extension;
      inherit installUrl;
    };
in
{
  config = lib.mkMerge [
    (lib.mkIf (piEnabled && !firefoxEnabled && isDarwin) {
      environment.casks = [
        (pkgs.librewolf-macos.override {
          policies = browserCliPoliciesFor librewolfBrowserCliInstallUrl;
          inherit (micsSkills) browser-cli-extension;
        })
      ];
    })
    (lib.mkIf (piEnabled && firefoxEnabled && isDarwin && nixCasksFirefox != null) {
      environment.casks = [
        (pkgs.callPackage ../../packages/firefox-macos {
          firefox = nixCasksFirefox;
          inherit (micsSkills) browser-cli-extension;
          policies = browserCliPoliciesFor firefoxBrowserCliInstallUrl;
        })
      ];
    })
  ];
}
