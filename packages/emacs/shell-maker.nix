{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "shell-maker";
  version = "0.96.1-unstable-2026-08-06";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "bb5e3aef17686c1c859c366eb83831b0046dc75a";
    sha256 = "sha256-+zVA2rbXXOISbKbugnp4MuEsPBCf/MJd/5jgPySsnoc=";
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
