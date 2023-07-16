{ lib, pkgs }:
let
  stdenv = pkgs.clangStdenv;
in
stdenv.mkDerivation rec {
  pname = "cpu-helper";
  version = "0.0.1";

  src = ./helper;

  buildPhase = ''
    make
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp helper $out/bin/sketchybar-cpu-helper
  '';

  meta = with lib; {
    description = "cpu helper";
    homepage = "https://github.com/FelixKratz/dotfiles/tree/master/.config/sketchybar/helper";
    platforms = platforms.darwin;
    license = licenses.mit;
  };
}
