{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ultra-scroll";
  version = "0.4.2-unstable-2025-11-22";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "ultra-scroll";
    rev = "203178269451177e07d19f1e200a726217b6b436";
    sha256 = "sha256-zT8Uy0oqhDwC7TKJuSrF4TRDBEnDdDNWWuHC2DzH48Q=";
  };
}
