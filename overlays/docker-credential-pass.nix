self: super:
with super; {
  docker-credential-pass = super.stdenv.mkDerivation rec {
    name = "docker-credential-pass";
    version = "0.6.3";
    src = super.fetchurl {
      url = "https://github.com/docker/docker-credential-helpers/releases/download/v0.6.3/docker-credential-pass-v0.6.3-amd64.tar.gz";
      sha256 = "04r3xxbfrylaah1i81b8722kkxgy71llqzf1p6fkppkdca96rzar";
    };

    nativeBuildInputs = [
      unzip
    ];


    # Work around the "unpacker appears to have produced no directories"
    # case that happens when the archive doesn't have a subdirectory.
    setSourceRoot = "sourceRoot=`pwd`";

    installPhase = ''
        mkdir -p $out/bin
        chmod -R u+w docker-credential-pass
        cp docker-credential-pass $out/bin/
      '';
    meta = {
      homepage = https://github.com/docker/docker-credential-helpers/;
      description = "docker-credential-helpers is a suite of programs to use native stores to keep Docker credentials safe.";
    };
  };
}
