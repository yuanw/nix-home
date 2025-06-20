{
  lib,
  melpaBuild,
  fetchFromGitHub,
  writeText,
  elpa2nix,
  melpa2nix,

  ...
}:
let
  inherit (lib) readFile;
in
(melpaBuild (_finalAttrs: {
  pname = "eaf-pdf-viewer";
  version = "0-unstable-2025-06-13";

  src = fetchFromGitHub {
    owner = "emacs-eaf";
    repo = "eaf-pdf-viewer";
    rev = "96fa83176f17a33e516bdb4e0abaa832e3328248";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-JEvIw9JL4P6+za9twhSBDlHLC3VABiR/Ovqjww8jU8E=";
  };

  elpa2nix = writeText "elpa2nix.el" ''
    ${readFile elpa2nix}
    (defun byte-recompile-directory (&rest _))
  '';
  melpa2nix = writeText "melpa2nix.el" ''
    ${readFile melpa2nix}
    (defun byte-recompile-directory (&rest _))
  '';
})).overrideAttrs
  {
    # Override genericBuild's postInstall, which tries to native-compile Elisp
    # files.
    # TODO: mv only the needed files
    postInstall = ''
      DST=$out/share/emacs/site-lisp/elpa/$ename-$melpaVersion/

      mv * $DST

    '';
  }
