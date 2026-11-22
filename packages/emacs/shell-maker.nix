{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "shell-maker";
  version = "0.98.3-unstable-2026-11-18";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "cba42efd503bbdf19a7244b9119efd8764e99ecb";
    sha256 = "sha256-MYdTYstR+9HOvxR0YllBDfia5P0T+l30fH9jC62a8hg=";
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
