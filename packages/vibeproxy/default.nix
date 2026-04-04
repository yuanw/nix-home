{
  stdenv,
  lib,
  fetchzip,
}:
stdenv.mkDerivation {
  pname = "vibeproxy";
  version = "1.8.130";

  src = fetchzip {
    url = "https://github.com/automazeio/vibeproxy/releases/download/v1.8.130/VibeProxy-arm64.zip";
    hash = "sha256-zBTsEDrOYmtgMu1y3YEr3hTNQdbhhqB4+ugfnaSHXV0=";
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
