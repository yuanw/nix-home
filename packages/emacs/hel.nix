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
  version = "0.12.0-unstable-2026-03-16";
  rev = "7706d7a5adbbef9d604d30f7e86a9322a7936447";
in

melpaBuild {
  pname = "hel";
  inherit version;

  src = fetchFromGitHub {
    owner = "helheim-emacs";
    repo = "hel";
    inherit rev;
    sha256 = "sha256-VWT90d1NCh68fTyJ5R5s5ZDCNDeVSWhGY1WNxXLfhTc=";
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
