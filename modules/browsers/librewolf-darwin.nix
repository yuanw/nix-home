# macOS: nixpkgs LibreWolf via Home Manager Apps with enterprise policies.
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

  hmLibrewolfApp = "${config.my.homeDirectory}/Applications/Home Manager Apps/LibreWolf.app";
  hmLibrewolfExe = "${hmLibrewolfApp}/Contents/MacOS/librewolf";
in
{
  options.modules.browsers.librewolf.darwinBrowserExe = lib.mkOption {
    type = lib.types.str;
    readOnly = true;
    visible = false;
    description = "LibreWolf Mach-O binary when installed via Home Manager on macOS.";
  };

  config = lib.mkIf (cfg.enable && isDarwin) {
    modules.browsers.librewolf.pkg = librewolfPkg;
    modules.browsers.librewolf.darwinBrowserExe = hmLibrewolfExe;
  };
}
