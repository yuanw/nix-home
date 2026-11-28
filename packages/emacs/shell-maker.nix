{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "shell-maker";
  version = "0.98.5-unstable-2026-11-24";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "6745635b4f5e62c3fe07a0f09b8dd4d92b79839d";
    sha256 = "sha256-pro1UiOt8hX7ykXIRRoss8v+frEhsiEV/We/6xl37UU=";
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
