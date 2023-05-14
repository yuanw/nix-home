{ lib
, mkDerivation
, base
, brick
, cmdargs
, directory
, file-embed
, random
, split
, text
, time
, vty
, word-wrap
}:
mkDerivation {
  pname = "gotta-go-fast";
  version = "0.3.0.8";
  sha256 = "0b056xnhq64knpvwjnkqicgny4g8pa1nbq811miwmkgv9w1g76kc";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    brick
    cmdargs
    directory
    file-embed
    random
    split
    text
    time
    vty
    word-wrap
  ];
  description = "A command line utility for practicing typing";
  license = lib.licenses.bsd3;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "gotta-go-fast";
}
