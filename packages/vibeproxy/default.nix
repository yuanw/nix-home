{
  stdenv,
  lib,
  fetchzip,
}:
stdenv.mkDerivation rec {
  pname = "vibeproxy";
  version = "1.8.132";

  src = fetchzip {
    url = "https://github.com/automazeio/vibeproxy/releases/download/v${version}/VibeProxy-arm64.zip";
    hash = "sha256-6SxML/z53znTUCPm/mmLy66BCFwJ8gpnAGKZgfVj6cY=";
    stripRoot = false;
  };

  phases = [ "installPhase" ];

  installPhase = ''
    mkdir -p $out/Applications
    cp -r $src/*.app "$out/Applications/"
  '';

  meta = {
    description = "HTTP proxy for AI-assisted coding workflows";
    homepage = "https://github.com/automazeio/vibeproxy";
    license = lib.licenses.unfree;
    platforms = [ "aarch64-darwin" ];
  };
}
