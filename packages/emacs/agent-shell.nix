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
  version = "0.50.1-unstable-2026-04-12";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "eb5a379c1db4e395a04acd1f16a6dfb92e803d60";
    sha256 = "sha256-BkWFrS++asLU33mVrFA5hMNqnTbWXqWIYXGHmhrYooE=";
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
