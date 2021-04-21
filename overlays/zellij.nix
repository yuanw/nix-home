{ lib, stdenv, fetchurl }:

stdenv.mkDerivation rec {
  pname = "zellij";
  version = "0.5.0-beta";

  src = fetchurl {
    url =
      "https://github.com/zellij-org/zellij/releases/download/v0.5.0-beta/zellij-macos-x86_64.tar.gz";
    sha256 = "0c738vkw61xq9sp3xs1fj3vypr34lvvdbdzhbpgpyih7wjqgns2s";
  };

  installPhase = ''
    mkdir -p $out
    cp -R * $out/
  '';

  libPath = lib.makeLibraryPath [ stdenv.cc.cc ];

  dontStrip = true;

  meta = with lib; {
    description =
      "A minimal, blazing fast, and extremely customizable prompt for any shell";
    homepage = "https://starship.rs";
    license = licenses.isc;
    maintainers = with maintainers; [
      bbigras
      davidtwco
      Br1ght0ne
      Frostman
      marsam
    ];
  };
}
