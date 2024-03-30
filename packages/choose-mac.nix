{ lib
, stdenv
, fetchFromGitHub
, AppKit
, Carbon
, CoreVideo
, SkyLight
, xcbuild
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "choose-mac";
  version = "1.3.1";

  src = fetchFromGitHub {
    owner = "chipsenkbeil";
    repo = "choose";
    rev = "${finalAttrs.version}";
    sha256 = "sha256-oR0GgMinKcBHaZWdE7O+mdbiLKKjkweECKbi80bjW+c=";
  };

  buildInputs = [
    AppKit
    Carbon
    CoreVideo
    xcbuild
    SkyLight
  ];

  makeFlags =
    if stdenv.isAarch64 then [
      "choose-arm64"
    ] else [
      "choose-x86_64"
    ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp choose $out/bin/choose

    runHook postInstall
  '';


  meta = {
    description = "A lightweight tool designed to add colored borders to user windows on macOS 14.0+";
    homepage = "https://github.com/FelixKratz/JankyBorders";
    license = lib.licenses.gpl3;
    mainProgram = "choose";
    platforms = lib.platforms.darwin;
  };
})
