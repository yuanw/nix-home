# exllamav3 1.5.1.post1 (vcruz305's fork) — the EXL3 engine behind
# RED-SNOW-5.3-Flash 2.49bpw on DGX Spark, packaged natively instead of
# through the deployment repo's Docker image (build.sh: FROM
# nvcr.io/nvidia/pytorch:26.07-py3, TORCH_CUDA_ARCH_LIST=12.1, `pip
# install --no-build-isolation -e .` — a 15-20 min compile for sm_121).
#
# Unlike TensorFold, exllamav3 does not JIT-compile at first start:
# setup.py precompiles the `exllamav3_ext` CUDAExtension (torch
# cpp_extension) at install time whenever torch is importable and
# EXLLAMA_NOCOMPILE is unset. So this derivation reproduces build.sh's
# build environment in the Nix sandbox, and the .so is baked into
# site-packages for good:
#
#   CUDA_HOME     merged redist tree (nvcc + crt/cudart/cccl headers +
#                 the cublas/curand/cusparse/cusolver libs). Several CUDA
#                 packages keep headers in separate outputs, so every
#                 output is merged — getDev alone misses them.
#   TORCH_CUDA_ARCH_LIST = 12.1   GB10 / sm_121, exactly the image's value.
#   MAX_JOBS = 16                the image's job count.
#   CPATH        pybind11 headers (nixpkgs torch does not vendor them
#                 into torch/include; cpp_extension doesn't add the
#                 python package's include dir).
#
# Dependencies mirror pyproject [project].dependencies minus
# triton-windows (a platform marker). `ninja` is listed there too, but
# it is only torch cpp_extension's build tool, so it rides
# nativeBuildInputs instead of the python dependency set.
# transformers (>= 5.0, for the GLM-5.3 tokenizer + chat template) is a
# *server* need, not an engine one — modules/red-snow.nix adds it.
{
  lib,
  buildPythonPackage,
  fetchFromGitHub,
  setuptools,
  symlinkJoin,
  ninja,
  cudaPackages,
  torch,
  tokenizers,
  numpy,
  rich,
  typing-extensions,
  safetensors,
  pillow,
  pyyaml,
  marisa-trie,
  pydantic,
  llguidance,
  pybind11,
}:

let
  inherit (import ./src.nix { inherit fetchFromGitHub; }) src;

  # CUDA_HOME for the AOT extension build: cpp_extension wants
  # bin/nvcc, include/cuda_runtime.h and the libraries under one root.
  # Same merge as modules/tensorfold.nix (its JIT needed the same tree).
  getAllOutputs = p: map (o: p.${o}) p.outputs;
  cudaHome = symlinkJoin {
    name = "cuda-merged-${cudaPackages.cudaMajorMinorVersion}";
    paths = builtins.concatMap getAllOutputs (
      with cudaPackages;
      [
        cuda_nvcc
        cuda_crt # include/crt/host_config.h — cuda_runtime.h needs it
        cuda_cudart
        cuda_cccl
        cuda_nvrtc
        cuda_nvtx
        libcublas
        libcurand
        libcusparse
        libcusolver
      ]
    );
  };
in
buildPythonPackage {
  pname = "exllamav3";
  version = "1.5.1.post1";
  pyproject = true;

  inherit src;

  build-system = [ setuptools ];

  dependencies = [
    torch
    tokenizers
    numpy
    rich
    typing-extensions
    safetensors
    pillow
    pyyaml
    marisa-trie
    pydantic
    llguidance
  ];

  nativeBuildInputs = [
    cudaHome # bin/nvcc on PATH
    ninja # torch cpp_extension's build system
  ];

  env = {
    # GB10 is sm_121; the container image set exactly this.
    TORCH_CUDA_ARCH_LIST = "12.1";
    MAX_JOBS = "16";
    CUDA_HOME = cudaHome;
    CPATH = "${pybind11}/include";
  };

  # Importing exllamav3(_ext) pulls in CUDA + GPU plumbing: no CPU checks.
  doCheck = false;
  pythonImportsCheck = [ ];

  meta = {
    description = "EXL3 inference engine, vcruz305 fork (RED-SNOW's pin), AOT CUDA build for DGX Spark sm_121";
    homepage = "https://github.com/vcruz305/exllamav3";
    license = lib.licenses.mit;
    platforms = [ "aarch64-linux" ];
  };
}
