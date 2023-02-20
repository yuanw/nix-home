final: prev:

{
  haskellPackages = prev.haskellPackages.override {
    overrides = haskellPackagesNew: haskellPackagesOld: rec {
      ws-access-token =
        haskellPackagesNew.callPackage ./ws-access-token/release.nix
        { };

      resource-id =
        haskellPackagesNew.callPackage ./resource-id/release.nix { };

      hi-chew =
        haskellPackagesNew.callPackage ./hi-chew/release.nix { };


    };
  };

}
