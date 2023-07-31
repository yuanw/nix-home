{ stdenv, fetchurl, unzip, lib }:
stdenv.mkDerivation rec {

  pname = "dart";
  version = "2.18.3";
  # version = "2.13.4";
  nativeBuildInputs = [ unzip ];

  src = fetchurl {
    url =
      "https://storage.googleapis.com/dart-archive/channels/stable/release/${version}/sdk/dartsdk-macos-x64-release.zip";
    sha256 = "15a9gzkvmlnw9g1dnkxssh0c6skwf7z40i176ck7hzagd5r71fjj";
    # sha256 = "0jqfivah4k0rfl867n5g1ai39k2ac7vgs7c58m7nbalrsd8kjklk";
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
