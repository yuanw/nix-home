self: super: {

  istio = super.stdenv.mkDerivation rec {
      name = "istio";
      version = "1.4.3";

      src = super.fetchurl {
        url = "https://github.com/istio/istio/releases/download/${version}/istioctl-${version}-osx.tar.gz";
        sha256 = "d04d7e845d62f5e8b5baadda1bf9c698ba00cc35345a03172444f3b5cdac327b";
      };
      unpackPhase = "";
      buildPhase = "";
      installPhase = ''
        mkdir -p $out/bin
        cp istioctl $out/bin
'';
      meta = {
        homepage = https://istio.io;
        description = "Connect, secure, control, and observe services";
        platforms = super.platforms.darwin;
      };
    };
}
