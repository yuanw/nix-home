{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "shell-maker";
  version = "0.89.2-unstable-2026-03-30";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "6377cbdb49248d670170f1c8dbe045648063583e";
    sha256 = "sha256-KeC3NN0wR3yxwekuLxwb9EsRQZoIcS1EK89yL/LGUWw=";
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
