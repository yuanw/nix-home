# nix-prefetch-github helheim-emacs hel-ghostel --rev <rev>
{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  hel,
  ghostel,
  dash,
  ...
}:

let
  version = "0.3.0-unstable-2026-08-26";
  rev = "999df8dfa84cb0074e8ae739262c1cbba9e3d3f3";
in

melpaBuild {
  pname = "hel-ghostel";
  inherit version;

  src = fetchFromGitHub {
    owner = "helheim-emacs";
    repo = "hel-ghostel";
    inherit rev;
    hash = "sha256-1NMGK6PBAKWdK/BCyQmmxBr2T4fx2yvU5wzbM4TSGL0=";
  };

  packageRequires = [
    hel
    ghostel
    dash
  ];

  recipe = writeText "recipe" ''
    (hel-ghostel
     :repo "helheim-emacs/hel-ghostel"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "Hel integration for Ghostel";
    homepage = "https://github.com/helheim-emacs/hel-ghostel";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
