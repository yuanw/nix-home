# macOS: Nix cask LibreWolf with shared enterprise policies.
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

  librewolfWithPolicies = pkgs.librewolf.override {
    extraPolicies = lib.foldl' lib.recursiveUpdate { } [
      searchPolicies
      privacyPolicies
      cfg.darwinExtraPolicies
    ];
  };

  librewolfPkg = pkgs.callPackage ../../packages/librewolf-darwin-signed.nix {
    librewolf = librewolfWithPolicies;
  };
in
{
  config = lib.mkIf (cfg.enable && isDarwin) {
    environment.casks = [ librewolfPkg ];
  };
}
