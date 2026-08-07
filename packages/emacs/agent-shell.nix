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
  version = "0.70.2-unstable-2026-08-07";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "5b8e2e3abf591ecad557f3bcf780bb05150fc0ec";
    sha256 = "sha256-LSeV8xpPuloh4XRh6HrYUwnozWlxh/J17DC53E+WCy8=";
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
