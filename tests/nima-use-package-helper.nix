{ pkgs, lib }:

let
  up = import ../modules/editor/emacs/nima/lib/use-package.nix { inherit lib; };

  fakeEpkgs = {
    sample = "sample-package";
    other = "other-package";
  };

  defaultFeature = up.mkUsePackageFeature "sample" { };
  overriddenFeature = up.mkUsePackageFeature "sample" {
    package = "other";
    noRequire = true;
  };
  functionPackageFeature = up.mkUsePackageFeature "sample" {
    package = epkgs: [
      epkgs.sample
      epkgs.other
    ];
  };
  groupedFeatures = up.mkUsePackageFeatures {
    sample = { };
    disabled = {
      enable = false;
      config = "(ignore)";
    };
  };
in
assert up.packageToList fakeEpkgs "sample" == [ "sample-package" ];
assert up.packageToList fakeEpkgs "missing" == [ ];
assert defaultFeature.epkgs fakeEpkgs == [ "sample-package" ];
assert overriddenFeature.epkgs fakeEpkgs == [ "other-package" ];
assert
  functionPackageFeature.epkgs fakeEpkgs == [
    "sample-package"
    "other-package"
  ];
assert lib.hasInfix "(use-package sample" defaultFeature.elisp;
assert lib.hasInfix ":no-require t" overriddenFeature.elisp;
assert groupedFeatures.features.sample.epkgs fakeEpkgs == [ "sample-package" ];
assert groupedFeatures.features.disabled.enable == false;
pkgs.runCommand "nima-use-package-helper-test"
  { meta.description = "Regression tests for nima use-package helper"; }
  ''
    mkdir -p "$out"
  ''
