{
  melpaBuild,
  fetchFromGitHub,
  writeText,
  lib,

  # Elisp dependencies

  # Native dependencies
  ...
}:

let
  pname = "home-row-expreg";
  version = "1.3.2-unstable-2025-08-27";
in
melpaBuild {
  inherit pname version;
  src = fetchFromGitHub {
    owner = "bommbo";
    repo = "home-row-expreg";
    #https://github.com/casouri/expreg/pull/9
    rev = "278c413dae4c5ea5fd9d8afe71ce2a8e4548238c";

    sha256 = lib.fakeSha256;

    # sha256 = "sha256-1s1Fo5oR5tUBC9JhXo76NBqqeoS+P7ID3PN7t3xLewI=";

  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp/elpa/$pname-$version
    cp -rv * $out/share/emacs/site-lisp/elpa/$pname-$version/
  '';

  recipe = writeText "recipe" ''
    (expreg
    :repo "bommbo/home-row-expreg"
    :files ("*.el")
    :fetcher github)
  '';

  packageRequires = [

  ];
}
