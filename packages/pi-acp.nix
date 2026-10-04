{
  lib,
  nodejs,
  buildNpmPackage,
  fetchFromGitHub,
}:
buildNpmPackage rec {
  pname = "pi-acp";
  version = "0.0.34-unstable-2026-09-24";

  src = fetchFromGitHub {
    owner = "svkozak";
    repo = "pi-acp";
    rev = "b0581c9c1d675e634234674484247008b03d69b4";
    hash = "sha256-QRwxOtTZOY+Np3PkAoy2o2PrUzEqjItM/372sCPlSMo=";
  };

  npmDepsHash = "sha256-/H/778yM5qmd8FxAoytZLebcqQ6kwZ+2qiAR6RUAYzs=";

  npmFlags = [
    "--ignore-scripts"
  ];

  makeWrapperArgs = [ "--prefix PATH : ${lib.makeBinPath [ nodejs ]}" ];

  doInstallCheck = false;

  meta = with lib; {
    description = "ACP adapter for pi coding agent";
    homepage = "https://github.com/svkozak/pi-acp";
    license = licenses.mit;
    mainProgram = "pi-acp";
    platforms = platforms.all;
  };
}
