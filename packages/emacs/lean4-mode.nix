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
  version = "1.1.2-unstable-2026-09-28";
  src = fetchFromGitHub {
    owner = "leanprover-community";
    repo = "lean4-mode";
    rev = "6d4030e494039383664a0c08348a5c137eb4641c";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-FzeGnxjtCpeYbDU3GzadCTnLGtDPoe1q8aTpaZBck7A=";
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
