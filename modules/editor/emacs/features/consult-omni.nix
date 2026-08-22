{ pkgs, lib, ... }:

let
  packagePath = ../../../../packages/emacs;
in
{
  epkgs = epkgs: [
    (pkgs.callPackage "${packagePath}/consult-omni" {
      inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
      inherit lib;
      inherit (epkgs)
        browser-hist
        consult
        consult-notes
        elfeed
        embark
        melpaBuild
        yequake
        ;
    })
  ];

}
