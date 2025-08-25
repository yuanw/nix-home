{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ultra-scroll";
  version = "0-unstable-2025-04-17";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "ultra-scroll";
    rev = "f2e4fba601b6116f6f0bcb73cadf897bd8f7b764";
    sha256 = "sha256-Dgt7eE4a1gi7iYAxLhfPbmo3Jglq97DJopf2i+Ft7vI=";
  };
}
