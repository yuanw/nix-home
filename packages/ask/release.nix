{
  mkDerivation,
  base,
  containers,
  mtl,
  fetchFromGitHub,

}:
mkDerivation {
  pname = "ask";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "msp-strath";
    repo = "ask";
    rev = "3cf318b5c486debfa3c22123284ab06cf95c8891";
    sha256 = "sha256-jAwl5H8ti0rzYXqn3nbjyqNugHKNic5WmU2xnxNzHCg=";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    containers
    mtl
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "ask";
}
