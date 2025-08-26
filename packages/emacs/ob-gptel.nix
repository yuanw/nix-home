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
  version = "0-unstable-2025-08-23";
  src = fetchFromGitHub {
    owner = "jwiegley";
    repo = "ob-gptel";
    rev = "60e704a390d767a7d06c8d3845ba8786b75f7da3";
    sha256 = "sha256-G3edExxR/Ebm2lcCBhqGEqUdAEDFVxOjqB7h0ulbilA=";

  };

  packageRequires = [
    org
    gptel

  ];
  preferLocalBuild = true;
  allowSubstitutes = false;

}
