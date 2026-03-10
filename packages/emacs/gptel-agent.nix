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
  version = "0-unstable-2026-03-08";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "c3612aee925f5bbe53d24d9ad2dc3e11e006962f";
    sha256 = "sha256-ACimZfoRh6y1P0E121jKv9tAvdk5E6hdtlsF68NHpwE=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}
