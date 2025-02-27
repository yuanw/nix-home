{
  mkDerivation,
  base,
  containers,
  lib,
  mtl,
  fetchFromGitHub,

}:
mkDerivation {
  pname = "ask";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "msp-strath";
    repo = "ask";
    # rev = "515f5cbb505e614f400c21f99bdbebd47609c659";
    sha256 = lib.fakeSha256;
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
