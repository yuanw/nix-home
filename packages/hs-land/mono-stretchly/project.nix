{ mkDerivation
, base
, data-default
, lens
, monomer
, text
, text-show
, time
}:
mkDerivation {
  pname = "mono-stretchly";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    base
    data-default
    lens
    monomer
    text
    text-show
    time
  ];
  license = "unknown";
  mainProgram = "mono-stretchly";
}
