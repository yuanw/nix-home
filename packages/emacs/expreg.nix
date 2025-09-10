{
  melpaBuild,
  fetchFromGitHub,
  writeText,

  # Elisp dependencies

  # Native dependencies
  ...
}:

let
  pname = "expreg";
  version = "1.3.2-unstable-2025-08-27";
in
melpaBuild {
  inherit pname version;
  src = fetchFromGitHub {
    owner = "bommbo";
    repo = "expreg";
    #https://github.com/casouri/expreg/pull/9
    rev = "e31c3da7f1df8db1de14ad6e40e2e0d0671e0970";

    sha256 = "sha256-1s1Fo5oR5tUBC9JhXo76NBqqeoS+P7ID3PN7t3xLewI=";

  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp/elpa/$pname-$version
    cp -rv * $out/share/emacs/site-lisp/elpa/$pname-$version/
  '';

  recipe = writeText "recipe" ''
    (expreg
    :repo "casouri/expreg"
    :files ("*.el")
    :fetcher github)
  '';

  packageRequires = [

  ];
}
