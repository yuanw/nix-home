{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies
  dash ? null,
  lsp-mode ? null,
  magit-section ? null,

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "lean4-mode";
  version = "1.1.2-unstable-2026-10-29";
  src = fetchFromGitHub {
    owner = "leanprover-community";
    repo = "lean4-mode";
    rev = "d66c4e9319fd9e6935e53785b8c6980e92040c46";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-zroBey9qeEZ7OgHEs/SPCKtrqSae4ViavtJLcmtbRdA=";
  };
  files = ''
    ("*.el"

     "data")
  '';
  packageRequires = [
    dash
    lsp-mode
    magit-section
  ];
  preferLocalBuild = true;
  allowSubstitutes = false;

}
