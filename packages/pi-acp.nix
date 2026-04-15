{
  lib,
  nodejs,
  buildNpmPackage,
  fetchFromGitHub,
}:
buildNpmPackage rec {
  pname = "pi-acp";
  version = "0.0.25";

  src = fetchFromGitHub {
    owner = "svkozak";
    repo = "pi-acp";
    rev = "c30957b38608d37b85be2a7d793606c9f37e8c83";
    hash = "sha256-MdEXjHvn8eCy2mPstgTwXUZh99whr8hCA4CTFis1h3g=";
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
