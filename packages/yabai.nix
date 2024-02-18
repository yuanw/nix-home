{ stdenv, fetchzip, lib, ... }:

stdenv.mkDerivation rec {
  pname = "yabai";
  version = "6.0.12";

  src = fetchzip {
    url =
      "https://github.com/koekeishiya/yabai/releases/download/v${version}/${pname}-v${version}.tar.gz";
    # hash = "sha256-w4MTHHYWwBq0/WkemYIeV49aA/DzFxXITD8gF5St0Yo=";
    hash = lib.fakeHash;
  };

  installPhase = ''
    mkdir -p $out/bin
    mkdir -p $out/share/man/man1/
    cp ./bin/yabai $out/bin/yabai
    cp ./doc/yabai.1 $out/share/man/man1/yabai.1
  '';

  meta = with lib; {
    description = ''
      A tiling window manager for macOS based on binary space partitioning
    '';
    homepage = "https://github.com/koekeishiya/yabai";
    platforms = platforms.darwin;
  };
}
