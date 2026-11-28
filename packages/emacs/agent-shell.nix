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
  version = "0.99.5-unstable-2026-11-27";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "3284e29d71ad4f4232c936c2530db4852a1aa174";
    sha256 = "sha256-nia1Z5TEsuC62lfH35AjlLc2qAY7qASZGSCNSWj9rVE=";
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
