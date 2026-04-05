{
  lib,
  buildPythonPackage,
  fetchFromGitHub,
  setuptools,
  wheel,
  dacite,
  huggingface-hub,
  librosa,
  mlx,
  numpy,
  typer,
}:

buildPythonPackage rec {
  pname = "parakeet-mlx";
  version = "0.5.1";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "senstella";
    repo = "parakeet-mlx";
    rev = "ba03a1b6e8df4edadc83aca312a32600831dd481";
    hash = "sha256-udiDBB8vp27ID1JRhT8rNj1S8agJslb2OVo5tkhnRLw=";
  };

  postPatch = ''
    substituteInPlace pyproject.toml \
      --replace-fail "dacite>=1.9.2" "dacite>=1.9.1"
  '';

  build-system = [
    setuptools
    wheel
  ];

  dependencies = [
    dacite
    huggingface-hub
    librosa
    mlx
    numpy
    typer
  ];

  pythonImportsCheck = [ "parakeet_mlx" ];

  meta = with lib; {
    description = "Nvidia's Parakeet ASR models for Apple Silicon using MLX";
    homepage = "https://github.com/senstella/parakeet-mlx";
    license = licenses.asl20;
    maintainers = [ ];
    platforms = [ "aarch64-darwin" ];
  };
}
