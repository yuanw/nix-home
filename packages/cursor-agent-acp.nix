{
  lib,
  nodejs,
  buildNpmPackage,
  fetchFromGitHub,
}:
buildNpmPackage rec {
  pname = "cursor-agent-acp";
  version = "0.7.1";

  src = fetchFromGitHub {
    owner = "blowmage";
    repo = "cursor-agent-acp-npm";
    rev = "caf3583f550b776475d5c7a129c584fb9be7d450";
    hash = "sha256-k1DBiBVyjKHnVsvXWACuWc1y9SqEp5mLT253L/xakkU=";
  };

  npmDepsHash = "sha256-prT/vy1jYBiQ+8lurk1kAeTvXeNj22ftQe3p+n9q0AM=";

  npmFlags = [
    "--ignore-scripts"
  ];

  makeWrapperArgs = [ "--prefix PATH : ${lib.makeBinPath [ nodejs ]}" ];

  doInstallCheck = false;

  meta = with lib; {
    description = "Bridge Cursor CLI with ACP-compliant editors (Zed, JetBrains IDEs)";
    homepage = "https://github.com/blowmage/cursor-agent-acp-npm";
    license = licenses.mit;
    mainProgram = "cursor-agent-acp";
    platforms = platforms.all;
  };
}
