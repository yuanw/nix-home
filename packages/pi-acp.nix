{
  lib,
  nodejs,
  buildNpmPackage,
  fetchFromGitHub,
}:
buildNpmPackage rec {
  pname = "pi-acp";
  version = "0.0.33-unstable-2026-07-30";

  src = fetchFromGitHub {
    owner = "svkozak";
    repo = "pi-acp";
    rev = "d1cffc047ab37a096ee70ca39cfc1de463db8d12";
    hash = "sha256-y8QE91ZbRxzoaV8ITw95OqUEpsxkTI9eicygEF1GUFc=";
  };

  npmDepsHash = "sha256-qN+b/tMbnJLkWjotl3XrA0nfZ3KT/mT6gM+n3Qiz8Wk=";

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
