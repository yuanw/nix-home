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
  version = "0-unstable-2026-03-18";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "33a391871596c997c3dbce7f9f14457a94dd6c3c";
    sha256 = "sha256-6OMzH/AScM2lqCBiB3nzNqYiN6WezQbDFT4RFKx+C5Q=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}
