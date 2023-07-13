{ stdenv, unzip, fetchurl, lib }:

stdenv.mkDerivation rec {
  pname = "font-hack-nerd-font";
  version = "3.0.2";

  src = fetchurl {
    url = "https://github.com//ryanoasis/nerd-fonts/releases/download/v${version}/Hack.zip";
    hash = "sha256-6eYgqlyaAq5gzEHpWb90spWS+e/WQjXtm5Or+kSGdKE=";
  };

  buildInputs = [ unzip ];
  sourceRoot = ".";

  installPhase = ''
    install_path=$out/share/fonts/truetype
    mkdir -p $install_path
    find -name "*.ttf" -exec cp {} $install_path \;
  '';

  meta = with lib; {
    description = ''
      sketchybar-app-font
    '';
    homepage = "https://github.com/kvndrsslr/sketchybar-app-font";
    platforms = platforms.darwin;
  };
}
