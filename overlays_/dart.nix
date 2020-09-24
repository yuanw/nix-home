self: super: {

dart = with super; stdenv.mkDerivation rec {

  pname = "dart";
  version = "2.9.3";

  nativeBuildInputs = [
    unzip
  ];

  src = fetchurl {
    url = "https://storage.googleapis.com/dart-archive/channels/stable/release/${version}/sdk/dartsdk-macos-x64-release.zip";
    sha256 = "1b5p6jn6fpk95nd6qjclwmdsjdywcs7nzvgzlqmcyjq2bfazk7zj";
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
