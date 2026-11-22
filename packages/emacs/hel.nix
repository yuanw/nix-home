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
  version = "new_undo_system-unstable-2026-11-16";
  rev = "68941b02448175b8da832054f34a515edc563ba0";
in

melpaBuild {
  pname = "hel";
  inherit version;

  src = fetchFromGitHub {
    owner = "helheim-emacs";
    repo = "hel";
    inherit rev;
    sha256 = "sha256-exRKnjhdMjLFT8eyMeT+u4xTmoRHXyERNhcoRWX20bU=";
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
