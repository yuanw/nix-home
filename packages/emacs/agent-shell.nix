{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  shell-maker,
  acp,
  ...
}:

melpaBuild {
  pname = "agent-shell";
  version = "0.50.1-unstable-2026-04-08";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "7f106a355295d6c0fde5cd589cf566df7850463f";
    sha256 = "sha256-GsgnU+nunQoXKMSPnhbxm5j34s2y7kFVolXKNJo8O+g=";
  };

  packageRequires = [
    shell-maker
    acp
  ];

  recipe = writeText "recipe" ''
    (agent-shell
     :repo "xenodium/agent-shell"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "AI agent shell for Emacs";
    homepage = "https://github.com/xenodium/agent-shell";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
