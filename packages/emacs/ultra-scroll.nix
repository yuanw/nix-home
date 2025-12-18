{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ultra-scroll";
  version = "0.5-unstable-2025-12-16";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "ultra-scroll";
    rev = "21c568b1a26e597714ad65b40f246dd6e9f71fdd";
    sha256 = "sha256-MV0FNI+Crqlj3sxGwNcaGXGw0Z9v1MX+cJOeyZx0MhA=";
  };
}
