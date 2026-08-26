# macOS: upstream LibreWolf DMG (nix-casks) with enterprise policies.
{
  config,
  lib,
  pkgs,
  inputs',
  ...
}:
let
  cfg = config.modules.browsers.librewolf;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;

  searchPolicies = import ../../packages/librewolf-search-policies.nix;
  privacyPolicies = import ../../packages/librewolf-privacy-policies.nix;

  enterprisePolicies = lib.foldl' lib.recursiveUpdate { } [
    {
      DontCheckDefaultBrowser = true;
      DisablePocket = true;
      DisableAppUpdate = true;
      DisableTelemetry = true;
    }
    searchPolicies
    privacyPolicies
    cfg.darwinExtraPolicies
  ];

  librewolfPkg = pkgs.callPackage ../../packages/librewolf-macos {
    librewolf = inputs'.nix-casks.packages.librewolf;
    policies = enterprisePolicies;
  };
in
{
  config = lib.mkIf (cfg.enable && isDarwin) {
    environment.casks = [ librewolfPkg ];
  };
}
