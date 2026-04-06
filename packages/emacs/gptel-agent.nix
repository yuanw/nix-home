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
  version = "0-unstable-2026-04-05";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "0d1534b203ea756c76d5161dfcd57ddc146f774e";
    sha256 = "sha256-lbLIkGcFI7PBC8H0dbU+kdpE2XD6gKdKrcO205mNgjA=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}
