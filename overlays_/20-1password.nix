self: super: {

  OnePassword-op = super.stdenv.mkDerivation rec {
    name = "1Password-op";
    version = "0.7.1";
    src = super.fetchurl {
      url = "https://cache.agilebits.com/dist/1P/op/pkg/v${version}/op_darwin_amd64_v${version}.zip";
      sha256 = "4708c3cc7f1ff1381be246cd47d798565087eaaeca7b042764fc7ca0a2f5901c";
      # date = 2019-10-04T11:16:19-0700;
    };
    buildInputs = [ self.unzip ];
    unpackPhase = ''
      unzip ${src}
    '';
    buildPhase = "";
    installPhase = ''
      mkdir -p $out/bin
      cp op $out/bin
    '';
  };

}
