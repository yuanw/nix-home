{
  lib,
  python3,
  gcc14Stdenv,
  fetchFromGitHub,
  ninja,
}:

let
  torch = python3.pkgs.torch;
  backendStdenv = gcc14Stdenv;
  cudaSupport = torch.cudaSupport or false;
  cudaPackages = if cudaSupport then torch.cudaPackages else { };
in

python3.pkgs.buildPythonPackage.override { stdenv = backendStdenv; } rec {
  pname = "flash-attn-4";
  version = "4.0.0.beta14";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "Dao-AILab";
    repo = "flash-attention";
    rev = "fa4-v4.0.0.beta14";
    fetchSubmodules = true;
    hash = "sha256-T8WBmuydGQrBqT5hXbxMh0DP8UT1ErJTq9+XOAwyivs=";
  };

  # Reduce parallelism to avoid OOM on DGX Spark (unified memory)
  preConfigure = ''
    export MAX_JOBS=1
    export NVCC_THREADS=2
  '';

  env = lib.optionalAttrs cudaSupport {
    FLASH_ATTENTION_SKIP_CUDA_BUILD = "FALSE";
    CC = "${backendStdenv.cc}/bin/cc";
    CXX = "${backendStdenv.cc}/bin/c++";
    TORCH_CUDA_ARCH_LIST = "8.0;9.0;10.0;11.0;12.0;12.1";
    FLASH_ATTN_CUDA_ARCHS = "80;90;100;110;120;121";
  };

  build-system = [
    ninja
    python3.pkgs.setuptools
  ]
  ++ lib.optionals cudaSupport [
    cudaPackages.cuda_nvcc
  ];

  buildInputs = lib.optionals cudaSupport [
    cudaPackages.cuda_cccl
    cudaPackages.libcublas
    cudaPackages.libcurand
    cudaPackages.libcusolver
    cudaPackages.libcusparse
    cudaPackages.cuda_cudart
  ];

  dependencies = [
    torch
  ];

  doCheck = false;

  pythonImportsCheck = [ "flash_attn" ];

  meta = {
    description = "Flash Attention 4 — fast and memory-efficient exact attention (sm121/GB10 support)";
    homepage = "https://github.com/Dao-AILab/flash-attention";
    license = lib.licenses.bsd3;
    platforms = lib.platforms.linux;
    broken = !cudaSupport;
    maintainers = [ ];
  };
}
