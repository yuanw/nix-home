{ melpaBuild
, fetchFromGitea
, writableTmpDirAsHomeHook
, mupdf
, writeText
, ...
}:

melpaBuild {
  version = "0.3.0";
  pname = "reader";

  src = fetchFromGitea {
    domain = "codeberg.org";
    owner = "MonadicSheep";
    repo = "emacs-reader";
    rev = "0.3.0";
    sha256 = "sha256-BpuWWGt46BVgQZPHzeLEbzT+ooR4v29R+1Lv0K55kK8=";
  };

  nativeCheckInputs = [
    # Executables
    mupdf
    writableTmpDirAsHomeHook
  ];
  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp/elpa/$pname-$version
    cp -rv * $out/share/emacs/site-lisp/elpa/$pname-$version/
  '';

  recipe = writeText "recipe" ''
    (reader

    :files ("*.el", "render")

  '';

}
