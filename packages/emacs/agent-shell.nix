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
  version = "0.50.1-unstable-2026-04-06";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "73718e228c2011b0f645ed4c6b3f2377965b1940";
    sha256 = "sha256-xI23zZZ+0IUEeYs2xrWGIWlk32/HIMrg6LImkYRTaEc=";
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
