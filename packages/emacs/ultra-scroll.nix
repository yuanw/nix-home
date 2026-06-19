{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ultra-scroll";
  version = "0.6.2-unstable-2026-06-16";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "ultra-scroll";
    rev = "5be267d2d92c230b4347e0769f584c71aec53589";
    sha256 = "sha256-N8+ITu25WLtw6sqYxFR2LvfQQCV4ASTYVwDTYN5m/lM=";
  };
}
