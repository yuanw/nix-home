{
  melpaBuild,
  fetchFromGitHub,
  # Elisp dependencies
  compat ? null,
  gptel ? null,

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "gptel-quick";
  version = "0-unstable-2025-06-01";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-quick";
    rev = "495b5e0b5348dbced1448bd12cbf8847e30b5175";
    sha256 = "sha256-xMrzeWG5L+MpGAhPFlV8KV+xa7CWC1D48osRrioGlsw=";
  };

  packageRequires = [
    compat
    gptel

  ];
  preferLocalBuild = true;
  allowSubstitutes = false;

}
