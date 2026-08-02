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
  version = "0.66.1-unstable-2026-08-01";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "ad993f740cc55ac84c08439d265333ba135e3b59";
    sha256 = "sha256-X9ZLkSiqATBihnlin0gRB/Bash21i9ZnWKQmi6QinYY=";
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
