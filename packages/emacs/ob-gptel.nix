{
  melpaBuild,
  fetchFromGitHub,
  # Elisp dependencies
  org ? null,
  gptel ? null,

  # Native dependencies
  ...
}:
melpaBuild {
  pname = "ob-gptel";
  version = "0-unstable-2026-10-27";
  src = fetchFromGitHub {
    owner = "jwiegley";
    repo = "ob-gptel";
    rev = "65646efa49c8ba15177fcc47a69be20e314c4488";
    sha256 = "sha256-ldhSZuDlcjcz2iSqTcE02G6o7YGcnADvGaqZ1EeFQ3Q=";

  };

  packageRequires = [
    org
    gptel
  ];
  preferLocalBuild = true;
  allowSubstitutes = false;

}
