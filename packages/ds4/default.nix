{
  lib,
  fetchFromGitHub,
  cudaPackages,
}:

cudaPackages.backendStdenv.mkDerivation {
  pname = "ds4";
  version = "0-unstable-2026-05-17";

  src = fetchFromGitHub {
    owner = "antirez";
    repo = "ds4";
    rev = "c9dd9499bfa57c1bbfbb4446eff963330ab5329b";
    hash = "sha256-sp9DxKOIRLp1gQdsuPidkXfW78C1dwJA5Mo1rNtbJt0=";
  };

  strictDeps = true;

  nativeBuildInputs = [
    cudaPackages.cuda_nvcc
  ];

  buildInputs = [
    cudaPackages.cuda_cudart
    cudaPackages.libcublas
    cudaPackages.cuda_cccl
  ];

  makeFlags = [
    "cuda-spark"
    "NVCC=nvcc"
    "NATIVE_CPU_FLAG="
    "NVCCFLAGS=-O3 --use_fast_math -Xcompiler -pthread"
    "CUDA_LDLIBS=-lm -Xcompiler -pthread -lcudart -lcublas"
  ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp ds4 ds4-server ds4-bench ds4-eval $out/bin/
    runHook postInstall
  '';

  meta = {
    description = "DeepSeek V4 Flash inference engine (CUDA build)";
    homepage = "https://github.com/antirez/ds4";
    license = lib.licenses.mit;
    platforms = [
      "aarch64-linux"
      "x86_64-linux"
    ];
    mainProgram = "ds4";
  };
}
