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
  version = "0.45.1-unstable-2026-03-09";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "41c4b2143a6b583f3f7e886e0b93943e0abced18";
    sha256 = "sha256-uqUSIrXVld+R4i9z4eYRrUDbS70oRdWwpmdhpROZkoE=";
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
