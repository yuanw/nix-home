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
  version = "0-unstable-2025-11-05";
  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel-quick";
    rev = "018ff2be8f860a1e8fe3966eec418ad635620c38";
    sha256 = "sha256-7a5+YQifwtVYHP6qQXS1yxA42bVGXmErirra0TrSSQ0=";
  };

  packageRequires = [
    compat
    gptel

  ];
  preferLocalBuild = true;
  allowSubstitutes = false;

}
