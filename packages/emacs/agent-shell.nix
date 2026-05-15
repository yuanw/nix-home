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
  version = "0-unstable-2026-05-15";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "46623144c75b97163c26be5549b9e0c4bc3052d4";
    sha256 = "sha256-LIbMfash7UNfb7ArNQ0BD+xs09JIt0Q7PNnkVA2ak7A=";
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
