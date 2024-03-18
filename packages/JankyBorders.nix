{ lib
, stdenv
, fetchFromGitHub
, AppKit
, Carbon
, CoreVideo
, SkyLight
}:

let
  inherit (stdenv.hostPlatform) system;
  target = {
    "aarch64-darwin" = "arm64";
    "x86_64-darwin" = "x86";
  }.${system} or (throw "Unsupported system: ${system}");
in
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

  makeFlags = [
    target
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
