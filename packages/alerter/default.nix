{
  stdenv,
  lib,
  fetchurl,
  unzip,
}:
stdenv.mkDerivation rec {

  pname = "alerter";
  version = "26.5";

  src = fetchurl {
    url = "https://github.com/vjeantet/alerter/releases/download/v${version}/alerter-${version}.zip";
    hash = "sha256-EfY83cm7P4VU7Zt2JjKhIM+nvuBePAnWVzSCPgnSTxA=";
  };

  nativeBuildInputs = [ unzip ];

  # zip contains a single file with no subdirectory
  setSourceRoot = "sourceRoot=$(pwd)";

  installPhase = ''
    mkdir -p $out/bin
    cp alerter $out/bin/alerter
    chmod +x $out/bin/alerter
  '';

  meta = {
    homepage = "https://github.com/vjeantet/alerter";
    description = "Send Mac OS X User Alerts (Notifications) from the command line";
    longDescription = ''
      alerter is a command-line tool to send Mac OS X User Alerts (Notifications).
      The program ends when the alert is activated or closed, writing the activated
      value to stdout, or a JSON object describing the alert event.
    '';
    license = lib.licenses.mit;
    mainProgram = "alerter";
    platforms = [ "aarch64-darwin" ];
  };
}
