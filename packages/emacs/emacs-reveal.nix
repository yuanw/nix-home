{

  trivialBuild,
  fetchFromGitLab,

}:
trivialBuild rec {
  pname = "emacs-reveal";
  version = "9.52.0";
  src = fetchFromGitLab {
    fetchSubmodules = true;
    owner = "oer";
    repo = "emacs-reveal";
    rev = "a201482cd7244cd736e614dd14a394e733509142";
    sha256 = "sha256-MCa28kGMBKLA/WqcDgJVtbul//R80nwWuI757wc12KI=";
  };

}
