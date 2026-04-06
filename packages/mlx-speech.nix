{
  lib,
  buildPythonPackage,
  fetchFromGitHub,
  uv-build,
  mlx,
  numpy,
  safetensors,
  soundfile,
  tokenizers,
}:

buildPythonPackage {
  pname = "mlx-speech";
  version = "0-unstable-2026-04-01";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "appautomaton";
    repo = "mlx-speech";
    rev = "d7bb3d79fe7b6cf545a79ca6ebfb0c22c221f6ad";
    hash = "sha256-083qJM0aXQmc0Yu+MW8a9MuzCDiye9AZHghP0Pgmr2s=";
  };

  postPatch = ''
    substituteInPlace pyproject.toml \
      --replace-fail "uv_build>=0.11.2,<0.12" "uv_build>=0.6"
  '';

  build-system = [ uv-build ];

  dependencies = [
    mlx
    numpy
    safetensors
    soundfile
    tokenizers
  ];

  pythonImportsCheck = [ "mlx_speech" ];

  meta = with lib; {
    description = "MLX-native speech synthesis library for Apple Silicon";
    homepage = "https://github.com/appautomaton/mlx-speech";
    license = licenses.mit;
    maintainers = [ ];
    platforms = [ "aarch64-darwin" ];
  };
}
