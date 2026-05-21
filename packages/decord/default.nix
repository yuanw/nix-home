{
  lib,
  stdenv,
  python3,
  ffmpeg_4,
  fetchFromGitHub,
  cmake,
  pkg-config,
}:

let
  version = "0.6.0";
  # commit with ffmpeg6 compatibility fix (PR #301)
  srcRev = "38f59c34799071a20076a1d16e9ea400fa40595a";
  srcHash = "sha256-5aOZnq0VvWjeD6rOeu+cCiF22G4J4OJie0J5UXtvsTI=";

  libdecord = stdenv.mkDerivation {
    pname = "libdecord";
    inherit version;

    src = fetchFromGitHub {
      owner = "dmlc";
      repo = "decord";
      rev = srcRev;
      hash = srcHash;
      fetchSubmodules = true;
    };

    nativeBuildInputs = [
      cmake
      pkg-config
    ];
    buildInputs = [ ffmpeg_4 ];

    installPhase = ''
      mkdir -p $out/lib
      cp libdecord.so $out/lib
    '';
  };
in

python3.pkgs.buildPythonPackage {
  pname = "decord";
  inherit version;
  format = "setuptools";

  src = fetchFromGitHub {
    owner = "dmlc";
    repo = "decord";
    rev = srcRev;
    hash = srcHash;
    fetchSubmodules = true;
  };

  sourceRoot = "source/python";

  nativeBuildInputs = with python3.pkgs; [
    setuptools
    wheel
  ];

  propagatedBuildInputs = with python3.pkgs; [ numpy ];

  preBuild = ''
    mkdir -p build
    cp ${libdecord}/lib/libdecord.so build/
  '';

  doCheck = false;

  pythonImportsCheck = [ "decord" ];

  meta = with lib; {
    description = "Efficient video loader for deep learning with fast random access";
    homepage = "https://github.com/dmlc/decord";
    license = licenses.asl20;
    platforms = platforms.all;
    maintainers = [ ];
  };
}
