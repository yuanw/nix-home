{
  stdenv,
  fetchurl,
  lib,
}:

stdenv.mkDerivation rec {
  pname = "sketchybar-app-font";
  version = "2.0.51";

  src = fetchurl {
    url = "https://github.com/kvndrsslr/sketchybar-app-font/releases/download/v${version}/sketchybar-app-font.ttf";
    hash = "sha256-Fp4jiSACIZskyn6T7ru21TtA4q78PLLqtWLjkOqyAq8=";
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
