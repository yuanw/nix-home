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
  version = "0-unstable-2026-07-17";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "e833bcaf617baf8c8075eac098231c4457386814";
    sha256 = "sha256-gX3n3T/jmmvA25s8qMpOyfQzuSKyY3OgkEkitMXfspg=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}
