{
  melpaBuild,
  fetchFromGitea,
  writableTmpDirAsHomeHook,
  mupdf,
  writeText,
  ...
}:

melpaBuild {
  version = "0.2.7";
  pname = "reader";

  src = fetchFromGitea {
    domain = "codeberg.org";
    owner = "divyaranjan";
    repo = "emacs-reader";
    rev = "b81d08af119f50ac0032e0dc7df260c9e6212be7";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-r14/ZzGq/+R0xhw0AjfWMatlgC8S9s4RAqTrlhld1eM=";

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
