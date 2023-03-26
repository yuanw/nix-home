{ mkDerivation, base, data-default, lens, lib, monomer, text
, text-show
}:
mkDerivation {
  pname = "mono-stretchly";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base data-default lens monomer text text-show
  ];
  enableSeparateDataOutput = true;
  license = lib.licenses.mit;
  mainProgram = "mono-stretchly";
}
