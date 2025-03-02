{
  mkDerivation,
  base,
  containers,
  mtl,
  fetchFromGitHub,
  lib,
}:
mkDerivation {
  pname = "ask";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "msp-strath";
    repo = "ask";
    rev = "51ee4b6b377685e6ec4bb7bd4b90d3c54b58598f";
    sha256 = lib.fakeSha256;
    #sha256 = "sha256-Uhz5fJctlV1NSFUkA/MidX/K1YYql3tVzTtA+aFcgvY=";

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
