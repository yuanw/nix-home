{ trivialBuild
, fetchFromGitHub
,
}:
trivialBuild rec {
  pname = "auto-save";
  version = "0.0.1";
  src = fetchFromGitHub {

    owner = "manateelazycat";
    repo = "auto-save";
    rev = "0fb3c0f38191c0e74f00bae6adaa342de3750e83";
    sha256 = "sha256-MCa28kGMBKLA/WqcDgJVtbul//R80nwWuI757wc12KI=";
  };

}
