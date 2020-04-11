self: super: {

  knative = super.stdenv.mkDerivation rec {
    name = "knative";
    version = "0.13.1";

    src = super.fetchurl {
      url = "https://github.com/knative/client/releases/download/${version}/kn-darwin-amd64";
      sha256 = "199b98gg5s2z1b6dy5ixsbync8vrn648dsv99xkd62hfphk78n28";
    };
    dontUnpack = true;
    buildPhase = "";
    installPhase = ''
      mkdir -p $out/bin
      cp ${src} $out/bin/kn
      chmod +x $out/bin/kn
    '';
    meta = {
      homepage = https://istio.io;
      description = "Connect, secure, control, and observe services";
    };
  };
}
