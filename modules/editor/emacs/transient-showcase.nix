{ trivialBuild
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
    sha256 = "sha256-YH3qlezGsi9FtF4OocwF/K4MG+GVrv4tw53E6gjwtok=";
  };
  # elisp dependencies
  propagatedUserEnvPkgs = [
    transient
  ];
  buildInputs = propagatedUserEnvPkgs;
}
