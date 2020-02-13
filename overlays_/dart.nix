self: super: {

dart = with super; stdenv.mkDerivation rec {

  pname = "dart";
  version = "2.7.1";

  nativeBuildInputs = [
    unzip
  ];

  src = fetchurl {
    url = "https://storage.googleapis.com/dart-archive/channels/stable/release/${version}/sdk/dartsdk-macos-x64-release.zip";
    sha256 = "4eb7f0fd651bcafe3cf0df8530eea5891ff7aebb5d88f83cf104d808e5983a0b";
  };


  installPhase = ''
    mkdir -p $out/dart
    cp -R * $out/dart/

     # create wrappers with correct env
    for program in dart dart2js dart2native dartanalyzer dartaotruntime dartdevc dartdoc dartfmt pub; do
        programPath="$out/dart/bin/$program"
        binaryPath="$out/bin/$program"
        mkdir -p $out/bin
        ln -s $programPath $binaryPath
    done
  '';

  libPath = super.stdenv.lib.makeLibraryPath [ super.stdenv.cc.cc ];

  dontStrip = true;

  meta = {
    homepage = https://www.dartlang.org/;
    description = "Scalable programming language, with robust libraries and runtimes, for building web, server, and mobile apps";
    longDescription = ''
      Dart is a class-based, single inheritance, object-oriented language
      with C-style syntax. It offers compilation to JavaScript, interfaces,
      mixins, abstract classes, reified generics, and optional typing.
    '';
    license = stdenv.lib.licenses.bsd3;
    platforms = ["x86_64-darwin" ];
  };
};
}
