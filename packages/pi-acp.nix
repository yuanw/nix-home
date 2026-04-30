{
  lib,
  nodejs,
  buildNpmPackage,
  fetchFromGitHub,
}:
buildNpmPackage rec {
  pname = "pi-acp";
  version = "0.0.26-unstable-2026-04-24";

  src = fetchFromGitHub {
    owner = "svkozak";
    repo = "pi-acp";
    rev = "399a758fb6aa6229385aae6ee7a08a8b8814fed0";
    hash = "sha256-8FUQTGSaej/PhfbZsrKvSTpcObs5TI0LBzt73if5KhE=";
  };

  npmDepsHash = "sha256-GuHvjqSD4M87cGBtFFSF37FWF79+6pLlai0A99Ii/hM=";

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
