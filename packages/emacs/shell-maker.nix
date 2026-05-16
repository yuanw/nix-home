{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "shell-maker";
  version = "0-unstable-2026-05-12";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "11f4a9913e7625f122625dd89d668ad5c93cf151";
    sha256 = "sha256-4xJ4X7CuosPA0PEjiKRcfdR8mj7vUkGnajUIDBV+oEk=";
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
