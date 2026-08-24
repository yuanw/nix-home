{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  agent-shell,
  knockknock,
  ...
}:

melpaBuild {
  pname = "agent-shell-knockknock";
  version = "0.0.1-unstable-2026-03-16";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell-knockknock";
    rev = "56732434067fe1874dcda62c491f7800bdc0a2f3";
    sha256 = "sha256-R7bvk2v9togbPiGKOXDAKCMStrL1bbRJoTdBtj0PdlU=";
  };

  packageRequires = [
    agent-shell
    knockknock
  ];

  recipe = writeText "recipe" ''
    (agent-shell-knockknock
     :repo "xenodium/agent-shell-knockknock"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "Knockknock notifications for agent-shell";
    homepage = "https://github.com/xenodium/agent-shell-knockknock";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
