{
  lib,
  nodejs,
  buildNpmPackage,
  fetchFromGitHub,
}:
buildNpmPackage rec {
  pname = "pi-acp";
  version = "0.0.34-unstable-2026-10-06";

  src = fetchFromGitHub {
    owner = "svkozak";
    repo = "pi-acp";
    rev = "04d0a15d6bb42bb65cce5c9caedcdcb2cdcb6905";
    hash = "sha256-V9+WNpJQK2a/S3e5KVXfiK1bhQlXrUKIaowhOZa6BIg=";
  };

  npmDepsHash = "sha256-BvLNtFfp1cMVjzWcMRSdhTqiJrTfbFoUbWkkPW9200o=";

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
