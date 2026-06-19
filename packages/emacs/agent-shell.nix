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
  version = "0.55.1-unstable-2026-06-12";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "6a2184f2b941e8cedbf980e62f85a49c759159c0";
    sha256 = "sha256-luPm96F/89aapfSVyYWHKZ6USGwRnjIzbwDcH9SxSVo=";
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
