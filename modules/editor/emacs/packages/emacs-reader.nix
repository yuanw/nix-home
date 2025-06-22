{
  melpaBuild,
  fetchFromGitea,
  writableTmpDirAsHomeHook,
  mupdf,
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
    #hash = lib.fakeSha256;
    hash = "sha256-8wNq9a7yNbpkuOH7oMRO1kbdAQb87HXeMHvOamYSwyQ=";
  };

  nativeCheckInputs = [
    # Executables
    mupdf
    writableTmpDirAsHomeHook
  ];

  files = ''
    ("*.el"
     "render")
  '';

}
