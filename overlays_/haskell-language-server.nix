self: super: {

  haskell-language-server = with super;
    stdenv.mkDerivation rec {

      pname = "haskell-language-server";
      version = "0.4.0";

      sourceRoot = ".";

      dontConfigure = true;

      buildInputs = [ gzip ];

      srcs = [
        (fetchurl {
          url =
            "https://github.com/haskell/haskell-language-server/releases/download/0.4.0/haskell-language-server-macOS-8.6.5.gz";
          sha256 = "12q1sl3pippx2jqpbnr8d8zc33xflrvwk398519n1vvjdqkx4yf8";
        })
        (fetchurl {
          url =
            "https://github.com/haskell/haskell-language-server/releases/download/0.4.0/haskell-language-server-wrapper-macOS.gz";
          sha256 = "1mkbm5nw3hfwrsmqdhjh5j0f6v2j10gaj5r28j615kx4ayc8xnf1";
        })
      ];

      phases = [ "unpackPhase" "installPhase" ];

      unpackPhase = ''
        runHook preUnpack
        for _src in $srcs; do
          if [ -f "$_src" ]; then
             gunzip $_src
          else
             echo "$_src doesnt exist"
          fi
        done
        runHook postUnpack
      '';

      installPhase = ''
        runHook preInstall
        mkdir -p $out/bin
        cp haskell-language-server-wrapper-macOS $out/bin/
        runHook postInstall
      '';

      meta = {
        homepage = "https://github.com/haskell/haskell-language-server/";
      };
    };
}
