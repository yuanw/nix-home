{ lib
, stdenv
, fetchFromGitHub
, AppKit
, Carbon
, CoreVideo
, SkyLight
, xcbuild
}:
stdenv.mkDerivation (_finalAttrs: {
  pname = "choose-mac";
  version = "1.3.1";

  src = fetchFromGitHub {
    owner = "chipsenkbeil";
    repo = "choose";
    rev = "29b07318a8c1e929c8b84e0e7a936763eabf2f89";
    sha256 = lib.fakeSha;
    # sha256 = "sha256-oR0GgMinKcBHaZWdE7O+mdbiLKKjkweECKbi80bjW+c=";
  };

  buildInputs = [
    AppKit
    Carbon
    CoreVideo
    xcbuild
    SkyLight
  ];

  makeFlags = [ "release" ];

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
