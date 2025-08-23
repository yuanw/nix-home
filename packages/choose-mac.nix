{
  stdenv,
  lib,
  fetchurl,
}:

stdenv.mkDerivation rec {
  pname = "choose-mac";

  version = "1.5.0";
  src = fetchurl {
    url = "https://github.com/chipsenkbeil/choose/releases/download/${version}/choose";
    sha256 = "sha256-ftfokSZkypsPOacUqsFknxHkShj8oc9vTjDythAz7uI=";
  };

  dontBuild = true;
  dontUnpack = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp $src $out/bin/choose
    chmod +x $out/bin/choose

    runHook postInstall
  '';

  meta = with lib; {
    maintainers = with maintainers; [ ];
    homepage = "https://github.com/chipsenkbeil/choose";
    license = licenses.mit;
    platforms = platforms.darwin;
  };
}
