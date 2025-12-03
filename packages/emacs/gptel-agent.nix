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
  version = "0-unstable-2025-11-26";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "320c6327a16d0792409f6901234dbdf9611cc613";
    sha256 = "sha256-n9+2MeZowbdwisU4OG2hOeAoGLm2PLlpfLr/1/BeSI0=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}
