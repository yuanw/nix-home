{
  trivialBuild,
  fetchFromGitLab,
  lib,

}:
trivialBuild rec {
  pname = "auto-save";
  version = "9.37.4";
  src = fetchFromGitLab {
    fetchSubmodules = true;
    owner = "oer";
    repo = "emacs-reveal";
    rev = "a201482cd7244cd736e614dd14a394e733509142";
    sha256 = lib.fakeSha256;
    # sha256 = "sha256-MCa28kGMBKLA/WqcDgJVtbul//R80nwWuI757wc12KI=";
  };

}
