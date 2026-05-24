{
  lib,
  python3,
  fetchFromGitHub,
  ninja,
}:

let
  torch = python3.pkgs.torch;
  # CUDA 13.2 supports g++ 15 (max CUDA_GCC_VERSIONS['13.0'] = 16.0),
  # so no gcc14 workaround needed — the default stdenv (gcc 15) is fine.
  cudaSupport = torch.cudaSupport or false;
  cudaPackages = if cudaSupport then torch.cudaPackages else { };
in

python3.pkgs.buildPythonPackage rec {
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
    export MAX_JOBS=4
    export NVCC_THREADS=4
  '';

  env = lib.optionalAttrs cudaSupport {
    FLASH_ATTENTION_SKIP_CUDA_BUILD = "FALSE";
    # Build for all supported arches including sm121 (GB10 Blackwell)
    FLASH_ATTN_CUDA_ARCHS = "120;121";
  };

  nativeBuildInputs = [
    ninja
    python3.pkgs.setuptools
  ]
  ++ lib.optionals cudaSupport [
    cudaPackages.cuda_nvcc
  ];

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
