{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ultra-scroll";
  version = "0.7-unstable-2026-08-02";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "ultra-scroll";
    rev = "e8c0a938bd03971ffd8beefbf01481e7a136b594";
    sha256 = "sha256-WlUQ0utr7bRMBJnIM2sphl9n4PPeD9ynt5lK1l4I3vc=";
  };
}
