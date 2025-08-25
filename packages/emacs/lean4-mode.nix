{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies
  dash ? null,
  lsp-mode ? null,
  # Native dependencies
  ...
}:
melpaBuild {
  pname = "lean4-mode";
  version = "1.1.2-unstable-2025-06-01";
  src = fetchFromGitHub {
    owner = "leanprover-community";
    repo = "lean4-mode";
    rev = "1388f9d1429e38a39ab913c6daae55f6ce799479";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-6XFcyqSTx1CwNWqQvIc25cuQMwh3YXnbgr5cDiOCxBk=";
  };
  files = ''
    ("*.el"

     "data")
  '';
  packageRequires = [
    dash
    lsp-mode
  ];
  preferLocalBuild = true;
  allowSubstitutes = false;

}
