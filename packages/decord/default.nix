{
  lib,
  python3,
  fetchurl,
}:

let
  version = "3.3.0";
  wheel = fetchurl {
    url = "https://files.pythonhosted.org/packages/e8/37/947bc17d6a16f5c678ab2c6ba3330b20f617ec7652f103051881cf1d98d9/decord2-3.3.0-cp313-cp313-manylinux_2_28_aarch64.whl";
    hash = "sha256-3q3Rf8HMMVUXD1ocT3VS0IUPLmdjJg9p4HMLEtpDQxM=";
  };
in

python3.pkgs.buildPythonPackage {
  pname = "decord";
  inherit version;
  format = "wheel";

  src = wheel;

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
