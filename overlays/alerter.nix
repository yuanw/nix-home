_: super: {

  alerter = with super;
    stdenv.mkDerivation rec {

      pname = "alerter";
      version = "002";

      buildInputs = [ unzip ];

      src = fetchurl {
        url =
          "https://github.com/vjeantet/alerter/releases/download/002/alerter_v002_darwin_amd64.zip";
        sha256 = "0jgcp89rvzzfcybqhwah50ripfp41q1v3zv8d69qp466kk99lxka";
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
        license = lib.licenses.bsd3;
        platforms = [ "x86_64-darwin" ];
      };
    };
}
