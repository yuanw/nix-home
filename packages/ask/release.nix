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
    rev = "51ee4b6b377685e6ec4bb7bd4b90d3c54b58598f";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-ydr/AKbaiphSl19oLsWt7QL9T0jBOdCnaX+aL3MkU/A=";

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
