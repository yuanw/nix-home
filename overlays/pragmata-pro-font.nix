# https://github.com/ahmedelgabri/dotfiles/blob/main/nix/pkgs/pragmatapro.nix
{ lib, stdenv, requireFile, unzip }:

stdenv.mkDerivation rec {

  name = "pragmatapro-${version}";
  version = "0.829";

  src = requireFile rec {
    name = "PragmataPro${version}.zip";
    url = "file://${name}";
    # nix-hash --flat --base32 --type sha256 /path/to/file
    sha256 = "0b1zyzh67fgjkx948m7f5gbvrnp7bzz830wlww2b1zg2q4w2qf7w";
    # sha256 = "187skl3ac8rp4k0jd8dcgdn4kwksdd12044isccxnwq3cajhlay1";
    message = ''
      ${name} font not found in nix store, to add it run:
      $ nix-store --add-fixed sha256 /path/to/${name}

      don't forget to git-crypt unlock

      Did you change the file? maybe you need to update the sha256
      $ nix-hash --flat --base32 --type sha256 /path/to/${name}'';
  };
  buildInputs = [ unzip ];
  phases = [ "unpackPhase" "installPhase" ];
  pathsToLink = [ "/share/fonts/truetype/" ];
  sourceRoot = ".";
  installPhase = ''
    install_path=$out/share/fonts/truetype
    mkdir -p $install_path
    find -name "PragmataPro*.ttf" -exec cp {} $install_path \;
  '';

  meta = with lib; {
    homepage = "https://www.fsd.it/shop/fonts/pragmatapro/";
    description = ''
      PragmataPro™ is a condensed monospaced font optimized for screen,
      designed by Fabrizio Schiavi to be the ideal font for coding, math and engineering
    '';
    platforms = platforms.all;
    licence = licences.unfree;
  };
}
