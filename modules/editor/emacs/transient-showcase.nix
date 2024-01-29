{ lib
, trivialBuild
, fetchFromGitHub
, transient
,
}:
trivialBuild rec {
  pname = "transient-showcase";
  version = "0.0.1";
  src = fetchFromGitHub {

    owner = "positron-solutions";
    repo = "transient-showcase";
    rev = "814b85eab2bbfffd827502ce830f72ff82fd3ae9";
    sha256 = lib.fakeSha256;
  };
  # elisp dependencies
  propagatedUserEnvPkgs = [
    transient
  ];
  buildInputs = propagatedUserEnvPkgs;
}
