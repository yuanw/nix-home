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
  version = "0.89.4-unstable-2026-10-29";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "14be85a5dfa9933c3a2970a83b24daea5c2b1a74";
    sha256 = "sha256-O6rzx8Tthk9sGvVegKawNWKZYLohBA1WqFtiEt1OiC0=";
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
