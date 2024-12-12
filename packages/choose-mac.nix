{
  stdenv,
  lib,
  fetchurl,
}:

stdenv.mkDerivation rec {
  pname = "choose-mac";

  version = "1.3.1";
  src = fetchurl {
    url = "https://github.com/chipsenkbeil/choose/releases/download/${version}/choose";
    sha256 = "1d0efb2e2a2c1d6a39830726ab7433da64cc22ca2eed19efd6678090cbb9e325";
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
