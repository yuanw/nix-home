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
  version = "0-unstable-2026-06-07";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-quick";
    rev = "36fe296e016449433fa1213f4b89cb8dc7d4db5e";
    sha256 = "sha256-W2cEtjhoXxAhMxycLAg0qe2Ehpgn1L/m1VcpZu/Trsw=";
  };

  packageRequires = [
    compat
    gptel

  ];
  preferLocalBuild = true;
  allowSubstitutes = false;

}
