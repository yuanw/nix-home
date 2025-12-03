{
  lib,
  nodejs,
  buildNpmPackage,
  fetchFromGitHub,
}:
buildNpmPackage rec {
  pname = "claude-code-acp";
  version = "0.11.0";

  src = fetchFromGitHub {
    owner = "zed-industries";
    repo = "claude-code-acp";
    rev = "v${version}";
    sha256 = "sha256-lOzXJJvNUq5gJEpmYIUgS0llXPCCwPjtcxHrn9PbTpc=";
  };

  npmDepsHash = "sha256-1a4XVnem5HeEwlj8gGO2Qq5gkoAsL+VJS7zU5MM5ptY=";
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
