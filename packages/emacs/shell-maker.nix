{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "shell-maker";
  version = "0.87.1-unstable-2026-03-10";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "86fbe695085fd59c7b65799da84a3e917ebf4528";
    sha256 = "sha256-fjqVzBoDYdWuaOigCjndYUXEPBElX/7CYTD7YEQciFI=";
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
