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
  version = "0-unstable-2026-05-09";
  src = fetchFromGitHub {
    owner = "jwiegley";
    repo = "ob-gptel";
    rev = "71584eb30e8317cf36104cec78b6d53c4433cae7";
    sha256 = "sha256-cSbhEeAOitGbbq5Ep8axypALc0ueuVKwk/uIfrXaG1g=";

  };

  packageRequires = [
    org
    gptel
  ];
  preferLocalBuild = true;
  allowSubstitutes = false;

}
