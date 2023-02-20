{ mkDerivation, aeson, base, bytestring, containers, cryptonite
, dhall, hedgehog, jwt, lib, req, text, time, vector
}:
mkDerivation {
  pname = "ws-access-token";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers cryptonite dhall jwt req text time
    vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hedgehog ];
  homepage = "https://github.com/yuanwang-wf/ws-access-token";
  description = "Project synopsis";
  license = lib.licenses.bsd3;
  mainProgram = "ws-access-token";
}
