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
  version = "0-unstable-2026-06-05";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "2853a579154cb4528082a372db79ecdec1eb17ad";
    sha256 = "sha256-nUxoLb9JAnM7v8Jj6jx1KC7KLFDMrz3V98Uz8n7I6C0=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}
