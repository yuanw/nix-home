{
  melpaBuild,
  fetchFromGitHub,
  # Elisp dependencies
  gptel ? null,
  yaml ? null,
  orderless ? null,
  ...
}:
melpaBuild {
  pname = "gptel-agent";
  version = "0-unstable-2026-08-24";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "326e0abb8c6db0e5c4c421bd9af495932ae94b62";
    sha256 = "sha256-2eqY1MebzsDOpf3YEHy6o31TbpLA2iIPZMMVrscLYMk=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}
