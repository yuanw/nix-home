{ stdenv, unzip, fetchurl, lib }:
stdenv.mkDerivation rec {

  pname = "alerter";
  # big sur version
  version = "1.0.1";

  # nativeBuildInputs = [ unzip ];

  # src = fetchurl {
  #   url = "https://github.com/vjeantet/alerter/releases/download/1.0.1/alerter_v1.0.1_darwin_amd64.zip";
  #     # "https://github.com/vjeantet/alerter/releases/download/${version}/alerter_v${version}_darwin_amd64.zip";
  #   sha256 = "sha256-gWHzn1CBcpKrGRD1pi3M/kOEuQShXvBnavBlgg3KLVo=";
  # };
  # # Work around the "unpacker appears to have produced no directories"
  # # case that happens when the archive doesn't have a subdirectory.
  # # setSourceRoot = "sourceRoot=`pwd`";

  #  buildPhase = ''
  #    unzip $src
  # '';


  installPhase = ''
    mkdir -p $out
    cp ${./alerter} $out/alerter-bin
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
    platforms = [ "x86_64-darwin" "aarch64-darwin" ];
  };

  dontUnpack = true;
}
