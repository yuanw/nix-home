{
  lib,
  nodejs,
  buildNpmPackage,
  fetchFromGitHub,
}:
buildNpmPackage rec {
  pname = "pi-acp";
  version = "0.0.33-unstable-2026-09-24";

  src = fetchFromGitHub {
    owner = "svkozak";
    repo = "pi-acp";
    rev = "c6a813f492fae7d3bfb41a7e8c977b63d5a6df19";
    hash = "sha256-c8yug9C5qdk57qhItVQG8zJXW5EJyQpKBPLvcImc0lk=";
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
