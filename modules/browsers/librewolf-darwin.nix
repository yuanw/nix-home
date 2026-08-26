# macOS: nixpkgs LibreWolf synced to /Applications/Nix Casks with enterprise policies.
{
  config,
  lib,
  pkgs,
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

  librewolfPkg = pkgs.librewolf.override {
    extraPolicies = enterprisePolicies;
  };
in
{
  config = lib.mkIf (cfg.enable && isDarwin) {
    environment.casks = [ librewolfPkg ];
  };
}
