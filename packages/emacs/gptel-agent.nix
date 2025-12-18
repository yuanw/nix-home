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
  version = "0-unstable-2025-12-10";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "99a8b940271fbe68cdfb7c2329d090dc4ef04b99";
    sha256 = "sha256-nscp0e2Tt6CNaSM5c0butfkbLAiS0IuCAiJQYJRvu64=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}
