{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "shell-maker";
  version = "0.87.1-unstable-2026-03-09";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "afd5509b99b28589bab3a0d06786118905eadba9";
    sha256 = "sha256-MwkKC7ad7dWk1Rdv9p2y96wKIBayhwQO+4/d/gZLsaM=";
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
