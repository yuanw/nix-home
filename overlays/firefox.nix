{ stdenv, pkgs, fetchurl, undmg }:

stdenv.mkDerivation rec {
  pname = "Firefox";
  version = "88.0.1";

  buildInputs = [ undmg ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p "$out/Applications"
    cp -r Firefox.app "$out/Applications/Firefox.app"
  '';

  src = fetchurl {
    name = "Firefox-${version}.dmg";
    url =
      "https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/en-GB/Firefox%20${version}.dmg";

    sha256 = "DyaHCoL+ZyuT3qx5HEmI6gJqnYdd12PCC3iXvwSCjA8=";
  };

  meta = with pkgs.lib; {
    description = "The Firefox web browser";
    homepage = "https://www.mozilla.org/en-GB/firefox";
    maintainers = [ maintainers.cmacrae ];
    platforms = platforms.darwin;
  };
}
