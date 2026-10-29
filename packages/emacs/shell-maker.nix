{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "shell-maker";
  version = "0.98.2-unstable-2026-10-29";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "b28ee61a2533e408ed180662f83bb84fc8f84ba6";
    sha256 = "sha256-i6550ftasw79P9EX1rbZ1Z00vDWL7kJpIpZZqZ9EC3E=";
  };

  packageRequires = [ ];

  recipe = writeText "recipe" ''
    (shell-maker
     :repo "xenodium/shell-maker"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "A shell maker library for Emacs";
    homepage = "https://github.com/xenodium/shell-maker";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
