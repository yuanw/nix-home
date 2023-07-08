{ stdenv, unzip, fetchurl, lib }:

stdenv.mkDerivation rec {
  pname = "sketchybar-app-font";
  version = "1.0.9";

  src = fetchurl {
    url = "https://github.com/kvndrsslr/sketchybar-app-font/releases/download/v${version}/sketchybar-app-font.ttf";
    hash = "sha256-3QEKNT026lhstJtHPsFA8f7/XXO7Bmjrth92dd9D2xw=";
   };

  buildCommand = ''
      install -m444 -Dt $out/share/fonts/truetype ${src}
    '';

  meta = with lib; {
    description = ''
      sketchybar-app-font
    '';
    homepage = "https://github.com/kvndrsslr/sketchybar-app-font";
    platforms = platforms.darwin;
  };
}
