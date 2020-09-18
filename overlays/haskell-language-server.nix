self: super: {

  haskell-language-server = with super;
    stdenv.mkDerivation rec {

      pname = "haskell-language-server";
      version = "0.4.0";

      buildInputs = [ gzip ];

      srcs = fetchFromGitHub {
        owner = "haskell";
        repo = "haskell-language-server";
        # The git tag to fetch
        rev = "${version}";
        # Hashes must be specified so that the build is purely functional
        sha256 = "0l53fm1j72hcaais2k8499gcgqndxy93nkbw4r5mkpm6hn2qa9xw";
      };


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
