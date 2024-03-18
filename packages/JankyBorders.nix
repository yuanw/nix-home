{ lib
, stdenv
, fetchFromGitHub
, AppKit
, Carbon
, CoreVideo
, SkyLight
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "jankyBorders";
  version = "1.6.0";

  src = fetchFromGitHub {
    owner = "FelixKratz";
    repo = "JankyBorders";
    rev = "v${finalAttrs.version}";
    hash = "sha256-DX1d228UCOI+JU+RxenhiGyn3AiqpsGe0aCtr091szs=";
  };

  buildInputs = [
    AppKit
    Carbon
    CoreVideo
    SkyLight
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp ./bin/borders $out/bin/borders

    runHook postInstall
  '';


  meta = {
    description = "A lightweight tool designed to add colored borders to user windows on macOS 14.0+";
    homepage = "https://github.com/FelixKratz/JankyBorders";
    license = lib.licenses.gpl3;
    mainProgram = "borders";
    platforms = lib.platforms.darwin;
  };
})
