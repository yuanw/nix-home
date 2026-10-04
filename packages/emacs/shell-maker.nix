{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "shell-maker";
  version = "0.97.5-unstable-2026-10-01";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "dcc05a8cf24eb62f2f611fd2d009a161fada5f6e";
    sha256 = "sha256-xSNKAA7p2hUGlxwkcN229LBaV3UcNnz56WECgJV4ViY=";
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
