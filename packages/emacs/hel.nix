{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "hel";
  version = "0.12.0";

  src = fetchFromGitHub {
    owner = "anuvyklack";
    repo = "hel";
    rev = "v${version}";
    sha256 = "sha256-1YXo9QXIxrvUa09Fh7zSMyIsxrFpFdizP4fWyBdDtbs=";
  };

  packageRequires = [
    pcre2el
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
