{
  melpaBuild,
  fetchFromGitHub,

  writableTmpDirAsHomeHook,
  mupdf,
  writeText,
  ...
}:

melpaBuild {
  version = "0.3.0-unstable-2025-07-28";
  pname = "reader";

  src = fetchFromGitHub {
    owner = "divyaranjan1905";
    repo = "emacs-reader";
    rev = "ae547d87c7a03eebc8fc00a820a85f8b64f8dfad";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-jvvJu/7lXZ2weHSEtQkJMNqGOkHOwDjxcfPzmSHBZEU=";

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
