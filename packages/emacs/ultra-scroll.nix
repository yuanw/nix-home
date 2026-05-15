{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ultra-scroll";
  version = "0-unstable-2026-05-15";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "ultra-scroll";
    rev = "a7df62e8b0eec1c1211af3715f9844218350762c";
    sha256 = "sha256-hr7O6rQj5Id1U5FFM1DRtyE0USf7LNzqvWawkBcEkao=";
  };
}
