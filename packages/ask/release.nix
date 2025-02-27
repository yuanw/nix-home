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
    owner = "yuanwang-wf";
    repo = "ask";
    rev = "7bbaae72b497183d7aaa91fa215e6ca9e3c90ea8";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-Uhz5fJctlV1NSFUkA/MidX/K1YYql3tVzTtA+aFcgvY=";

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
