{ stdenv, fetchurl, unzip, lib }:
stdenv.mkDerivation rec {

  pname = "dart";
  version = "2.12.1";

  nativeBuildInputs = [ unzip ];

  src = fetchurl {
    url =
      "https://storage.googleapis.com/dart-archive/channels/stable/release/${version}/sdk/dartsdk-macos-x64-release.zip";
    sha256 = "0s1m1fm7qrlf1h6gh5kvx0jwmjbhn7ws2gsfmv9vlyq1zr84pzgj";
  };

  installPhase = ''
    mkdir -p $out
    cp -R * $out/

    # create wrappers with correct env
    #for program in dart dart2js dart2native dartanalyzer dartaotruntime dartdevc dartdoc dartfmt pub; do
    #    programPath="$out/dart/bin/$program"
    #    binaryPath="$out/bin/$program"
    #    mkdir -p $out/bin
    #    ln -s $programPath $binaryPath
    #done
  '';

  libPath = lib.makeLibraryPath [ stdenv.cc.cc ];

  dontStrip = true;

  meta = {
    homepage = "https://www.dartlang.org/";
    description =
      "Scalable programming language, with robust libraries and runtimes, for building web, server, and mobile apps";
    longDescription = ''
      Dart is a class-based, single inheritance, object-oriented language
      with C-style syntax. It offers compilation to JavaScript, interfaces,
      mixins, abstract classes, reified generics, and optional typing.
    '';
    license = lib.licenses.bsd3;
    platforms = [ "x86_64-darwin" ];
  };
}
