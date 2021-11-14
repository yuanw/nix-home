{ stdenv, unzip, fetchurl, lib }:
stdenv.mkDerivation rec {

  pname = "alerter";
  # big sur version
  version = "004";

  buildInputs = [ unzip ];

  src = fetchurl {
    url =
      "https://github.com/vjeantet/alerter/releases/download/004/alerter_v004_darwin_amd64.zip";
    sha256 = "0wk8nzdkn30djdxz44za4jyl9z7m7c22a3v9ycgxn35yrrw3qmnz";
  };
  # Work around the "unpacker appears to have produced no directories"
  # case that happens when the archive doesn't have a subdirectory.
  setSourceRoot = "sourceRoot=`pwd`";

  installPhase = ''
    mkdir -p $out
    cp alerter $out/
  '';

  meta = {
    homepage = "https://github.com/vjeantet/alerter";
    description = "alerter";
    longDescription = ''
      alerter is a command-line tool to send Mac OS X User Alerts (Notifications),
      which are available in Mac OS X 10.8 and higher. (even catalina) the program ends when the alerter is activated or closed,
      writing a the activated value to output (stdout), or a json object to describe the alert event.
    '';
    license = lib.licenses.mit;
    platforms = [ "x86_64-darwin" ];
  };
}
