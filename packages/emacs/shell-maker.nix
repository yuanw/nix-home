{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "shell-maker";
  version = "0.97.3-unstable-2026-09-10";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "f448a74a8eded23aa42f8d60a41c5d8d3a183d07";
    sha256 = "sha256-wH0OYeKthy+V0pWX1WNM8BEJW/gkzEdj/duJfRScS0w=";
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
