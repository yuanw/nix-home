{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ultra-scroll";
  version = "0.6.2-unstable-2026-05-16";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "ultra-scroll";
    rev = "f38653053b5c9bbe8dbcb6b2236ab8997fc2f9bb";
    sha256 = "sha256-vTox9o5QLsBXKIi+Hru2CAmMXuTIPcxP2/fgiOO0Xio=";
  };
}
