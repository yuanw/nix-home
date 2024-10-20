{ melpaBuild
, fetchFromGitHub
, writeText
, unstableGitUpdater
, lib
  # Elisp dependencies

  # Native dependencies
, ...
}:

let

  pname = "ob-racket";
  version = "1.3.1";

in
melpaBuild {

  inherit pname version;
  src = fetchFromGitHub {
    owner = "hasu";
    repo = "emacs-ob-racket";
    rev = "c7b7eee58fcde2ad515b72288742e555e7ec7915";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-yv+PP1JyEvMxEToNbgDbgWih/GHdauwfYLzPaEPsEC8=";
  };

  dontConfigure = true;
  dontBuild = true;
  passthru.updateScript = unstableGitUpdater { };

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp/elpa/$pname-$version
    cp -rv * $out/share/emacs/site-lisp/elpa/$pname-$version/
  '';

  recipe = writeText "recipe" ''
    (ob-racket
    :repo "hasu/emacs-ob-racket"
    :files ("*.el" "*.rkt")
    :fetcher github)
  '';

  packageRequires = [


  ];
}
