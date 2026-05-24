{
  lib,
  python3,
  fetchFromGitHub,
  ninja,
  gcc14,
}:

let
  torch = python3.pkgs.torch;
  # Must match torch's CUDA version (12.9). Use gcc14 for g++ 15 compat.
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
    # Build for all supported arches (ensures best runtime perf)
    FLASH_ATTN_CUDA_ARCHS = "80;90;100;110;120;121";
    # CUDA 12.9 requires g++ < 15.0; nixpkgs unstable has gcc 15 by default.
    # Override host compiler to gcc14 so torch's cpp_extension version check
    # and nvcc both use a compatible compiler.
    CC = "${gcc14}/bin/gcc";
    CXX = "${gcc14}/bin/g++";
    CUDAHOSTCXX = "${gcc14}/bin/g++";
  };

  nativeBuildInputs =
    lib.optionals cudaSupport [ gcc14 ]
    ++ [
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
