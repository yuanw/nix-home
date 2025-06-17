{
  melpaBuild,
  fetchFromGitHub,
  lib,
  ...
}:
melpaBuild
  (_finalAttrs: {
    pname = "eaf-browser";
    version = "0-unstable-2025-06-13";

    src = fetchFromGitHub {
      owner = "emacs-eaf";
      repo = "eaf-pdf-viewer";
      rev = "96fa83176f17a33e516bdb4e0abaa832e3328248";
      sha256 = lib.fakeSha256;
      #sha256 = "sha256-DsPrctB1bSGBPQLI2LsnSUtqnzWpZRrWrVZM8lS9fms=";
    };
  }).overrideAttrs
  {
    # Override genericBuild's postInstall, which tries to native-compile Elisp
    # files.
    # TODO: mv only the needed files
    postInstall = ''
      DST=$out/share/emacs/site-lisp/elpa/$ename-$melpaVersion/

      mv * $DST

    '';
  }
