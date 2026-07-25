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
  version = "0.12.0";
in

melpaBuild {
  pname = "hel";
  inherit version;

  src = fetchFromGitHub {
    owner = "anuvyklack";
    repo = "hel";
    rev = "v${version}";
    sha256 = "sha256-1YXo9QXIxrvUa09Fh7zSMyIsxrFpFdizP4fWyBdDtbs=";
  };

  patches = [
    ./patches/hel-integration-z-prefix.patch
  ];

  packageRequires = [
    dash
    pcre2el
    avy
    ultra-scroll
  ];

  recipe = writeText "recipe" ''
    (hel
     :repo "anuvyklack/hel"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "Helix emulation layer for Emacs";
    homepage = "https://github.com/anuvyklack/hel";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
