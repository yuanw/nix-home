{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "shell-maker";
  version = "0-unstable-2026-06-01";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "adce7baa92d5715eaf2defc77eb394576eb226f1";
    sha256 = "sha256-Q4uMmqGz7PALGJpVFXQnDkvAJqhxeTbJpaUBYxkuzfk=";
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
