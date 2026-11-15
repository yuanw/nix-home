{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  pcre2el,
  dash,
  avy,
  ultra-scroll,
  ...
}:

let
  version = "new_undo_system-unstable-2026-11-08";
  rev = "21140350f834a5cce80401969eab75234244b166";
in

melpaBuild {
  pname = "hel";
  inherit version;

  src = fetchFromGitHub {
    owner = "helheim-emacs";
    repo = "hel";
    inherit rev;
    sha256 = "sha256-gr/YrbOysrxw0NV1XQwmJ9rdCnpjFd+I6KeJ4LSCsZs=";
  };

  packageRequires = [
    dash
    pcre2el
    avy
    ultra-scroll
  ];

  recipe = writeText "recipe" ''
    (hel
     :repo "helheim-emacs/hel"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "Helix emulation layer for Emacs";
    homepage = "https://github.com/helheim-emacs/hel";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
