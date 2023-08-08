{ mkDerivation
, base
, base64-bytestring
, bytestring
, lib
, optparse-applicative
, pretty-terminal
, text
}:
mkDerivation {
  pname = "resource-id";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    base64-bytestring
    bytestring
    optparse-applicative
    pretty-terminal
    text
  ];
  executableHaskellDepends = [
    base
    base64-bytestring
    bytestring
    optparse-applicative
    pretty-terminal
    text
  ];
  testHaskellDepends =
    [ base base64-bytestring bytestring pretty-terminal text ];
  homepage = "https://github.com/yuanwang-wf/resource-id#readme";
  license = lib.licenses.bsd3;
  mainProgram = "rid";
}
