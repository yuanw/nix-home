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
  version = "0.46.1-unstable-2026-03-10";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "586528b562eae7c0fb43b13a79b62429add7bf9d";
    sha256 = "sha256-zqTCAW8OO7Wh2n6pa/ZNiFd0tjki9tZ8Cywif7diGrk=";
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
