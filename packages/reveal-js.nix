{ stdenv, lib, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "reveal-js";
  version = "5.1.0";

  src = fetchFromGitHub {
    owner = "hakimel";
    repo = "reveal.js";
    rev = "${version}";
    sha256 = lib.fakeSha256;
    # sha256 = "187j7w2g0dv5c5d1q8hl1ldcy7j9w78ir7vklhmbpw5mkyaigdby";
  };
  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/
    cp -r dist $out/share/
    runHook postInstall

  '';
}
