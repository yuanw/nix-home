# macOS: LibreWolf from nix-casks with browser-cli extension policy baked in.
# Used when Firefox is not the primary browser (policies ship via HM for Firefox).
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
  browserCliPolicies = pkgs.callPackage ../../packages/browser-cli-policies.nix {
    inherit (micsSkills) browser-cli-extension;
  };
  useLibrewolf =
    config.modules.pi.enable
    && !(config.modules.browsers.firefox.enable or false)
    && pkgs.stdenv.isDarwin;
in
{
  config = lib.mkIf useLibrewolf {
    environment.casks = [
      (pkgs.librewolf-macos.override {
        policies = browserCliPolicies;
      })
    ];
  };
}
