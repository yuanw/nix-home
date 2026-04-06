{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ultra-scroll";
  version = "0.6-unstable-2026-03-26";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "ultra-scroll";
    rev = "0a9a26071ec33305cbc7a0f1dc7202702319d51f";
    sha256 = "sha256-KPDwZ5sfsDdZq6sShX2s5/u7UVjG3CNT2/C2jjmk52c=";
  };
}
