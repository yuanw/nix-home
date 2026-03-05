{
  melpaBuild,
  fetchFromGitHub,
  writeText,

  # Elisp dependencies

  # Native dependencies
  ...
}:

let
  pname = "hurl-mode";
  version = "0-unstable-2026-02-19";
in
melpaBuild {
  inherit pname version;
  src = fetchFromGitHub {
    owner = "JasZhe";
    repo = "hurl-mode";
    rev = "6608e69296d37f5e090030280270d40046f1fe8e";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-ypo2sX6If/qMGxR2vgDQtAnF2GtNx/dC/Kb8WeEu7hk=";
  };

  dontConfigure = true;
  dontBuild = true;

  postPatch = ''
    substituteInPlace hurl-mode.el \
      --replace ';;; hurl-mode.el --- Major mode for hurl' \
                ';;; hurl-mode.el --- Major mode for hurl  -*- lexical-binding: t; -*-'
  '';

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp/elpa/$pname-$version
    cp -rv * $out/share/emacs/site-lisp/elpa/$pname-$version/
  '';

  recipe = writeText "recipe" ''
    (hurl-mode
    :repo "jaszhe/hurl-mode"
    :files ("*.el")
    :fetcher github)
  '';

  packageRequires = [

  ];
}
