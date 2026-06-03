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
  version = "0-unstable-2026-06-03";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "2b09e4cdc487f23b33167f2ffe2082b0ead39582";
    sha256 = "sha256-ZgoB3tXlGkurpDhXG+lcrl3HUBX/KiRXGiJsbm78bAo=";
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
