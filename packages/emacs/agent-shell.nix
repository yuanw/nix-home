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
  version = "0.75.2-unstable-2026-09-08";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "cc24345e9db2a205f7b8821015f89be3de7dd1e4";
    sha256 = "sha256-++ik5k/+lDkv9PgQcuqom6gAO02N/21vWj6piU2eUbE=";
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
