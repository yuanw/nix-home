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
  version = "0-unstable-2026-04-15";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "e2ef97d6b566b2ad751c8a0a87b8272710c95808";
    sha256 = "sha256-XaZ8FCsR6f2yOr0qtkIpGT4J6Tf9JyO2lJMBYrZyCE0=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}
