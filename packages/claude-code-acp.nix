{
  lib,
  nodejs,
  buildNpmPackage,
  fetchFromGitHub,
}:
buildNpmPackage rec {
  pname = "claude-code-acp";
  version = "0.10.6";

  src = fetchFromGitHub {
    owner = "zed-industries";
    repo = "claude-code-acp";
    rev = "v${version}";
    sha256 = "sha256-sq4m4w8ZmgU7Quel5gUwmvq0rumo+G1SkCt7fQsg+8M=";
  };

  npmDepsHash = "sha256-ElxSaU74txRC/eH7S+Uv33Ji7y2HE6rZU2DmEPICTko=";
  dontNpmBuild = false;

  npmFlags = [
    "--ignore-scripts"
  ];

  makeWrapperArgs = [ "--prefix PATH : ${lib.makeBinPath [ nodejs ]}" ];

  # Version check disabled - command doesn't support --version flag
  doInstallCheck = false;

  meta = with lib; {
    description = "Use Claude Code from any ACP-compatible clients such as Zed";
    homepage = "https://github.com/zed-industries/claude-code-acp";
    license = licenses.asl20;
    mainProgram = "claude-code-acp";
    platforms = platforms.all;
  };
}
