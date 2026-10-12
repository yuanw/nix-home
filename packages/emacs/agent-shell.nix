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
  version = "0.87.7-unstable-2026-10-12";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "3e4b4ddf8a534ac287916663a246500444da284b";
    sha256 = "sha256-0axSyGDWEMcWUE8ZA+WbfxguMrk/tMuBsEd6iOUd4j4=";
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
