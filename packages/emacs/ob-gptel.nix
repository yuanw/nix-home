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
  version = "0-unstable-2026-03-16";
  src = fetchFromGitHub {
    owner = "jwiegley";
    repo = "ob-gptel";
    rev = "cbed018a7d81de9ba8dc3220e1c4d10b7bb29b11";
    sha256 = "sha256-mXGzu6W21dVZ/utwyUEZxSn5PbnsKtTkBGTVi//cyyM=";

  };

  packageRequires = [
    org
    gptel
  ];
  preferLocalBuild = true;
  allowSubstitutes = false;

}
