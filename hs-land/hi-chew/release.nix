{ mkDerivation, base, containers, dhall, lib, text, turtle, vector }:
mkDerivation {
  pname = "hi-chew";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers dhall text turtle vector ];
  license = "unknown";
  mainProgram = "hi-chew";
}
