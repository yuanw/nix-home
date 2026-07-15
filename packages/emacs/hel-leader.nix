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
in

melpaBuild {
  pname = "hel-leader";
  inherit version;

  src = fetchFromGitHub {
    owner = "anuvyklack";
    repo = "hel-leader";
    rev = "v${version}";
    sha256 = "sha256-uJ684ik1hUeRQv6uQPQx7urKfo3yqqt4X3dHwnUxGlI=";
  };

  packageRequires = [
    hel
    dash
    s
  ];

  recipe = writeText "recipe" ''
    (hel-leader
     :repo "anuvyklack/hel-leader"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "Leader key for Hel";
    homepage = "https://github.com/anuvyklack/hel-leader";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
