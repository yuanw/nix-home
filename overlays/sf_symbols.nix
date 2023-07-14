{ stdenv, fetchurl, config, lib, pkgs, ... }:
# https://github.com/Homebrew/homebrew-cask/blob/d5acc87dd9ebf3cc1a38f2e4f99c1c2872f5ceb6/Casks/sf-symbols.rb
stdenv.mkDerivation rec {
  pname = "sf-symbols";
  version = "5";

  src = fetchurl {
    url = "https://devimages-cdn.apple.com/design/resources/download/SF-Symbols-${version}.dmg";
    hash = "sha256-WXnmgGaoIn0IFSw457wvLtAKLnTBl5L/Rq5zMCPijnU=";
  };
  unpackPhase = ''
    undmg $src
    ls
    7z x 'SF Symbols.pkg'
    7z x 'Payload~'
  '';
  buildInputs = [ pkgs.p7zip pkgs.undmg ];
  installPhase = ''
    mkdir -p $out/share/fonts
    mkdir -p $out/share/fonts/opentype
    mkdir -p $out/share/fonts/truetype
    find -name \*.otf -exec mv {} $out/share/fonts/opentype/ \;
    find -name \*.ttf -exec mv {} $out/share/fonts/truetype/ \;
  '';
  meta = with lib; {
    description = ''
      SF-symbols
    '';
    platforms = platforms.darwin;
  };
}
