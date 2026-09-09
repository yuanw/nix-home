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
  version = "2.1";
  rev = "9e7bd67d6e1ce0915bfd5f2341eb0b9ea5217bbf";
in

melpaBuild {
  pname = "hel-leader";
  inherit version;

  src = fetchFromGitHub {
    owner = "helheim-emacs";
    repo = "hel-leader";
    inherit rev;
    sha256 = "sha256-uJ684ik1hUeRQv6uQPQx7urKfo3yqqt4X3dHwnUxGlI=";
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
