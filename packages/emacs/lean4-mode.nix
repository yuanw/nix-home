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
  version = "1.1.2-unstable-2026-09-23";
  src = fetchFromGitHub {
    owner = "leanprover-community";
    repo = "lean4-mode";
    rev = "d5ed4b1610de45d265fded03b9b1af904efd6c03";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-LCVc4+n6LyRtwiLzF4sMmgI3XEI6fesEJu1vHqsjfoI=";
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
