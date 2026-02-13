{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "auto-save";
  version = "0-unstable-2026-01-12";
  src = fetchFromGitHub {
    owner = "manateelazycat";
    repo = "auto-save";
    rev = "515a0f5b1c5d3c331a195811521414221d6f0bbe";
    sha256 = "sha256-KO2fYZQnXW4Bp4ahAQWSksYIxqSKJGRA1gX1DIxKr5w=";
  };
  preferLocalBuild = true;

}
