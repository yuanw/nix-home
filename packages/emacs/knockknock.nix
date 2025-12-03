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
  version = "0-unstable-2025-11-27";
  src = fetchFromGitHub {
    owner = "konrad1977";
    repo = "knockknock";
    rev = "025b451e7e499eeb43ea51feedda1d8b011660d8";
    sha256 = "sha256-0wiofhTNSdgCaa7kxukapfMnQ6HBUn9kwWj8HJdaMW0=";
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
