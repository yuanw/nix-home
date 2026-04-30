{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ultra-scroll";
  version = "0.6-unstable-2026-04-25";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "ultra-scroll";
    rev = "6dfb3478e6ee1a6c1534c56235c55f9d0ad9dca4";
    sha256 = "sha256-KsLbI9hbLxPc+aYhzeTenKhDJ8qrOxv5ZXzAbqsotEo=";
  };
}
