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
  version = "0-unstable-2026-07-30";
  src = fetchFromGitHub {
    owner = "jwiegley";
    repo = "ob-gptel";
    rev = "4961120b7fc6bd2e2debd73f84cdef360188d3c7";
    sha256 = "sha256-DiUFW0oh0rOEc0aUKUVh6i2tSYtuszCgqCJnDMV1XQQ=";

  };

  packageRequires = [
    org
    gptel
  ];
  preferLocalBuild = true;
  allowSubstitutes = false;

}
