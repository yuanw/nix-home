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
  version = "0.56.1-unstable-2026-06-19";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "bb1bcfa97231d35f498197ac647debcc128ee3c4";
    sha256 = "sha256-QlymnRCCdfqn/T1objK1rpegfJobZfK2VdJxX5m8K80=";
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
