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
  version = "0-unstable-2025-11-18";
  src = fetchFromGitHub {
    owner = "konrad1977";
    repo = "knockknock";
    rev = "2df92b46af156408230a86de775dd8cfa52981dd";
    sha256 = "sha256-EI1TG6UovaOVpN7l4DoQqyew1amc1DwTO9V4ACdOALA=";
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
