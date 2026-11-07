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
  version = "1.1.2-unstable-2026-10-31";
  src = fetchFromGitHub {
    owner = "leanprover-community";
    repo = "lean4-mode";
    rev = "32daa3e56120e6bfc88fcdfa297c4af04b3c08b0";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-Zn2f98bm9tykyg/T0TfP+KbiI+OVCT636anJ2+zkCuA=";
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
