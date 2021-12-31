{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "StevenBlack-hosts";
  version = "3.9.30";

  src = fetchFromGitHub {
    owner = "StevenBlack";
    repo = "hosts";
    rev = "${version}";
    sha256 = "187j7w2g0dv5c5d1q8hl1ldcy7j9w78ir7vklhmbpw5mkyaigdby";
  };
  installPhase = ''
    mkdir -p $out/share/hosts
    cp hosts $out/share/hosts
    mv alternates $out/share/hosts
  '';
}
