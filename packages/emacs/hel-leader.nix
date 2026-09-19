{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  hel,
  dash,
  s,
  ...
}:

let
  version = "2.1-unstable-2026-07-09";
  rev = "32230075e01749ace44ddf2d25fca0ba6aa98fbd";
in

melpaBuild {
  pname = "hel-leader";
  inherit version;

  src = fetchFromGitHub {
    owner = "helheim-emacs";
    repo = "hel-leader";
    inherit rev;
    sha256 = "sha256-2cJxCJWwnGWLyodYU4rbnnQ3uzV6oWl+zATVniraDSw=";
  };

  packageRequires = [
    hel
    dash
    s
  ];

  recipe = writeText "recipe" ''
    (hel-leader
     :repo "helheim-emacs/hel-leader"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "Leader key for Hel";
    homepage = "https://github.com/helheim-emacs/hel-leader";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
