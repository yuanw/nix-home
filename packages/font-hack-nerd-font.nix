{
  stdenv,
  unzip,
  fetchurl,
  lib,
}:

stdenv.mkDerivation rec {
  pname = "font-hack-nerd-font";
  version = "3.1.1";

  src = fetchurl {
    url = "https://github.com//ryanoasis/nerd-fonts/releases/download/v${version}/Hack.zip";
    hash = "sha256-XP+52rWuunKUyrizKo1QyCY/Y9YIOzhBHkAD0TQkBo4=";
    # hash = lib.fakeHash;
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
      eveloper targeted fonts with a high number of glyphs
    '';
    homepage = "https://github.com/ryanoasis/nerd-fonts";
    platforms = platforms.darwin;
  };
}
