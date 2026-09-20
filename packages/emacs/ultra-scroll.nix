{
  melpaBuild,
  fetchFromGitHub,

  # Elisp dependencies

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ultra-scroll";
  version = "0.7.1-unstable-2026-08-28";
  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "ultra-scroll";
    rev = "8aa8e7496b06fd7c3585fcae8275300a77e57730";
    sha256 = "sha256-tsJyMd0tOx6WGRDMOKn+J2wRtJA34njzJB5pNLXm9oE=";
  };
}
