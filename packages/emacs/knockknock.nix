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
  version = "0-unstable-2026-01-07";
  src = fetchFromGitHub {
    owner = "konrad1977";
    repo = "knockknock";
    rev = "0920b9b390d4b4cc0818574e115d87eb245a41b5";
    sha256 = "sha256-hTkXfXMIEL4GtZV177/6uFybdmH+zxwsDnaXSXvG4hs=";
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
