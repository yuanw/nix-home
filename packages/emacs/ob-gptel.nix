{
  melpaBuild,
  fetchFromGitHub,
  # Elisp dependencies
  org ? null,
  gptel ? null,

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ob-gptel";
  version = "0-unstable-2026-09-04";
  src = fetchFromGitHub {
    owner = "jwiegley";
    repo = "ob-gptel";
    rev = "db030ea033e9cce0db6457ba530d6f29e5d181f2";
    sha256 = "sha256-6oJQi5MwO2xG6SbIo10OJInP2qZep2Kp8WYfmvBbQ3w=";

  };

  packageRequires = [
    org
    gptel
  ];
  preferLocalBuild = true;
  allowSubstitutes = false;

}
