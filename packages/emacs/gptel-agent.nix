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
  version = "0-unstable-2026-05-02";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "aecaa940faf29107b4bc0a2bd621419c2d04b8d0";
    sha256 = "sha256-P8ftfJQeTgIalOvjdYqASHpOXuh1RLWL12dmVuJ/YSw=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}
