self: super: {

  istio = super.stdenv.mkDerivation rec {
    name = "istio";
    version = "1.5.1";

    src = super.fetchurl {
      url = "https://github.com/istio/istio/releases/download/1.5.1/istio-1.5.1-osx.tar.gz";
      sha256 = "089vyafxfdvlfpvvwihb0cgy2i2ipjb6fjrs3n1zzf4624h0avfz";
    };
    buildPhase = "";
    installPhase = ''
       mkdir -p $out/istio
       cp -R * $out/istio/

       # create wrappers with correct env
      for program in istioctl; do
          programPath="$out/istio/bin/$program"
          binaryPath="$out/bin/$program"
          mkdir -p $out/bin
          ln -s $programPath $binaryPath
      done

    '';
    buildInputs = [ super.unzip ];
    meta = {
      homepage = https://istio.io;
      description = "Connect, secure, control, and observe services";
      platforms = super.stdenv.lib.platforms.darwin;
    };
  };
}
