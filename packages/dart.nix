{
  stdenv,
  fetchurl,
  unzip,
  lib,
}:
stdenv.mkDerivation rec {

  pname = "dart";
  version = "2.19.6";
  nativeBuildInputs = [ unzip ];

  src = fetchurl {
    # https://storage.googleapis.com/dart-archive/channels/stable/release/2.19.6/sdk/dartsdk-macos-arm64-release.zip
    # https://storage.googleapis.com/dart-archive/channels/stable/release/2.19.6/sdk/dartsdk-macos-x64-release.zip
    url = "https://storage.googleapis.com/dart-archive/channels/stable/release/${version}/sdk/dartsdk-macos-arm64-release.zip";
    sha256 = "PGtUtvRLyji9x4WOpFc08peVHrpfsQyPp7hrSj9D7bY=";
    # sha256 = lib.fakeSha256;
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
    description = "Scalable programming language, with robust libraries and runtimes, for building web, server, and mobile apps";
    longDescription = ''
      Dart is a class-based, single inheritance, object-oriented language
      with C-style syntax. It offers compilation to JavaScript, interfaces,
      mixins, abstract classes, reified generics, and optional typing.
    '';
    license = lib.licenses.bsd3;
    platforms = [ "aarch64-darwin" ];
  };
}
