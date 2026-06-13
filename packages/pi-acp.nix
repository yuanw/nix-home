{
  lib,
  nodejs,
  buildNpmPackage,
  fetchFromGitHub,
}:
buildNpmPackage rec {
  pname = "pi-acp";
  version = "0.0.28-unstable-2026-06-12";

  src = fetchFromGitHub {
    owner = "svkozak";
    repo = "pi-acp";
    rev = "f9ca92d5e14ca5ed4ae3883031e2425bd517f87d";
    hash = "sha256-sGmP6HYHmz2QyACPWnM4vuhnIr8GnKLJXtj98tvTe74=";
  };

  npmDepsHash = "sha256-EmzhcvVzrirlKh57Tl4BKVG4XLkAgdaYgdhMfpZVbRI=";

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
