{
  melpaBuild,
  fetchFromGitHub,
  # Elisp dependencies
  magit ? null,
  # Native dependencies
  ...
}:
melpaBuild {
  pname = "magit-ai";
  version = "0-unstable-2026-03-16";
  src = fetchFromGitHub {
    owner = "jwiegley";
    repo = "magit-ai";
    rev = "3f7fce8ebb0ff5f2bbfaaea502231b3c62c1bbe2";
    sha256 = "sha256-AL3ehGl6+b/mQXBRt8RN12xPfJYZ8V45xECETFO1ksc=";
  };

  packageRequires = [ magit ];
  preferLocalBuild = true;
  allowSubstitutes = false;
}
