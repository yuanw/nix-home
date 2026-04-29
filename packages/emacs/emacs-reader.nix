{ melpaBuild
, fetchFromGitea
, writableTmpDirAsHomeHook
, mupdf
, writeText
, ...
}:

melpaBuild {
  version = "0.3.0-unstable-2025-07-28";
  pname = "reader";

  src = fetchFromGitea {
    domain = "codeberg.org";
    owner = "MonadicSheep";
    repo = "emacs-reader";
    rev = "9824fc91eb51bec0edb8c3634a74d73226d26525";
    sha256 = "sha256-84v8NzAjH0djD98RKElzy3dIkSSh1c3OyjrHXR8cQrY=";
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
