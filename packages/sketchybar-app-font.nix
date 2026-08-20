{
  stdenv,
  fetchurl,
  lib,
}:

stdenv.mkDerivation rec {
  pname = "sketchybar-app-font";
  version = "2.0.80";

  src = fetchurl {
    url = "https://github.com/kvndrsslr/sketchybar-app-font/releases/download/v${version}/sketchybar-app-font.ttf";
    hash = "sha256-mJ9wSfLQJT4y1DD2OFPTov2/GUHKkZi5X7B/B/hYDJQ=";
  };

  buildCommand = ''
    install -Dm444 ${src} $out/share/fonts/truetype/sketchybar-app-font.ttf
  '';

  meta = with lib; {
    description = ''
      sketchybar-app-font
    '';
    homepage = "https://github.com/kvndrsslr/sketchybar-app-font";
    platforms = platforms.darwin;
  };
}
