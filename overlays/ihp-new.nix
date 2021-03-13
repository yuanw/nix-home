{ stdenv, git, makeWrapper, fetchFromGitHub }:
stdenv.mkDerivation rec {

  buildInputs = [ git makeWrapper ];
  pname = "ihp-new";
  version = "0.8.0";

  src = fetchFromGitHub {
    owner = "digitallyinduced";
    repo = "ihp";
    rev = "v0.9.0";
    sha256 = "12bfmgw5ixsy89yd18xihyx310afhk7ydmrgjmql8jsaqkkw3s8m";
  };

  # We override the install phase, as the emojify project doesn't use make
  installPhase = ''
    # Make the output directory
    mkdir -p $out/bin;
    cp ProjectGenerator/bin/ihp-new $out;
    makeWrapper $out/ihp-new $out/bin/ihp-new --prefix PATH ":" "${git}/bin";
  '';
}
