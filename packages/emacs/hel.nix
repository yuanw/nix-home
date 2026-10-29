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
  version = "new_undo_system-unstable-2026-10-26";
  rev = "4d971b14fbccacd04c8ec9290b694c8ad1bacac6";
in

melpaBuild {
  pname = "hel";
  inherit version;

  src = fetchFromGitHub {
    owner = "helheim-emacs";
    repo = "hel";
    inherit rev;
    sha256 = "sha256-UeHY9cekmjgaUiQiSFFRXCAYOvd195ry4ZYwOWqvTlg=";
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
