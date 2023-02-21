{ mkDerivation, base, lib, turtle }:
mkDerivation {
  pname = "hi-chew";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base turtle ];
  license = "unknown";
  mainProgram = "hi-chew";
}
