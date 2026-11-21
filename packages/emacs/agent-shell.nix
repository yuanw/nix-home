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
  version = "0.98.4-unstable-2026-11-20";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "dde9392902d750375bedaf7347c31cc15c66ca8f";
    sha256 = "sha256-FOXguQBoufSEqxSVqTJF9tpV1UBNk8SxrT69QVVlEiE=";
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
