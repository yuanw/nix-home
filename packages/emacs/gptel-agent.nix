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
  version = "0-unstable-2026-04-09";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-agent";
    rev = "90aaaede809fa32167507032cddf1fbe769dcd7d";
    sha256 = "sha256-Hlyg3LxlxrYvRLELFRMBAq9TojogADYbDHU3+3QOzKg=";
  };

  packageRequires = [
    gptel
    yaml
    orderless
  ];

  preferLocalBuild = true;
  allowSubstitutes = false;

}
