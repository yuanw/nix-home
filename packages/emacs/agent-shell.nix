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
  version = "0.96.1-unstable-2026-11-13";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "26365eb6f2fc3a635eb10bfccc97b3f4264d64be";
    sha256 = "sha256-LKrka/NvhMjHNzrcuGpm2SgeFalXs0iMds0xHJ7hDoE=";
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
