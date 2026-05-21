{
  lib,
  python3,
  fetchurl,
  autoPatchelfHook,
  zlib,
}:

let
  version = "3.3.0";
  wheel = fetchurl {
    url = "https://files.pythonhosted.org/packages/e8/37/947bc17d6a16f5c678ab2c6ba3330b20f617ec7652f103051881cf1d98d9/decord2-3.3.0-cp313-cp313-manylinux_2_28_aarch64.whl";
    hash = "sha256-3q3RfMALZVRe9zH7X1jgXWJdyNtfvaJe3JvTBGk0NBM=";
  };
in

python3.pkgs.buildPythonPackage {
  pname = "decord";
  inherit version;
  format = "wheel";

  src = wheel;

  nativeBuildInputs = [ autoPatchelfHook ];

  buildInputs = [ zlib ];

  propagatedBuildInputs = with python3.pkgs; [ numpy ];

  doCheck = false;

  pythonImportsCheck = [ "decord" ];

  meta = with lib; {
    description = "Efficient video loader for deep learning with fast random access (FFmpeg8 compatible fork)";
    homepage = "https://github.com/johnnynunez/decord2";
    license = licenses.asl20;
    platforms = [ "aarch64-linux" ];
    maintainers = [ ];
  };
}
