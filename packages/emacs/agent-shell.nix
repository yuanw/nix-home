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
  version = "0.53.1-unstable-2026-06-08";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "13ce65c49c532f859925ed8b3d1e4565b258800a";
    sha256 = "sha256-7g65mf+mhk/PS6CNGbjopseDIfpnpIPXRnR6QGDc8aY=";
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
