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
  version = "0-unstable-2026-10-12";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "2fbe4f6c71a6a1355c647ce90151c8c2bc1c4bdf";
    sha256 = "sha256-gAmXGecE6c4zcNHFHmaiQdf740wgfMPkNDWV6dA0ky8=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}
