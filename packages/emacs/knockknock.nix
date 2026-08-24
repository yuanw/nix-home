{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies
  posframe ? null,
  nerd-icons ? null,
  ...
}:
melpaBuild {
  pname = "knockknock";
  version = "0-unstable-2026-03-16";
  src = fetchFromGitHub {
    owner = "konrad1977";
    repo = "knockknock";
    rev = "7a6ab46503554317b639a7333ec8046d7d181520";
    sha256 = "sha256-hkvEuad3Gh++PMeaMJHd2j//ho+FA59QGxCex3NVi98=";
  };

  files = ''
    ("knockknock.el"

     )
  '';
  propagatedUserEnvPkgs = [
    posframe
    nerd-icons
  ];
  packageRequires = [
    posframe
    nerd-icons
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}
