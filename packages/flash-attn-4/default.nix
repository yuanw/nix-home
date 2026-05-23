{
  lib,
  python3,
  gcc14Stdenv,
  fetchFromGitHub,
  ninja,
}:

let
  torch = python3.pkgs.torch;
  # Must match torch's CUDA version (12.9). Use gcc14Stdenv for g++ 15 compat.
  cudaSupport = torch.cudaSupport or false;
  cudaPackages = if cudaSupport then torch.cudaPackages else { };
in

python3.pkgs.buildPythonPackage.override { stdenv = gcc14Stdenv; } rec {
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

  preConfigure = ''
    export MAX_JOBS=1
    export NVCC_THREADS=2
  '';

  env = lib.optionalAttrs cudaSupport {
    FLASH_ATTENTION_SKIP_CUDA_BUILD = "FALSE";
    # Only build for sm121 (DGX Spark GB10). Skip cross-compilation.
    FLASH_ATTN_CUDA_ARCHS = "121";
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
