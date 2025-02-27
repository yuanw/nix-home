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
    rev = "799552ae72ec796815f835c669f8822d40eecd80";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-A+lwpzEiPoiu54ZFy7Pgyl/fKS3IwoSQwmKsSm71w3s=";
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
