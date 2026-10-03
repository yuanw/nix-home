# TensorFold v0.6.1 — the CUDA serving engine behind Qwen3.8 Flash Next
# on DGX Spark, packaged natively instead of through the podman recipe.
#
# The container (nvcr.io/nvidia/pytorch:26.07-py3 + pip install
# git+.../TensorFold@v0.6.1 + one site-packages patch) only ever
# supplied the *toolchain*: TensorFold's own ~25 .cu/.cpp kernels are
# JIT-compiled on first start (src/tensorfold/cuda/build.py uses
# torch.utils.cpp_extension.load, and the triton kernels compile into
# TRITON_CACHE_DIR). Natively the same works with the CUDA-enabled
# nixpkgs torch — the service unit (hosts/dgx-spark/configuration.nix)
# provides nvcc + ninja on PATH and a CUDA_HOME for those JIT builds.
#
# The single patch (patches/0002-flash-next-v061.patch) lives in the
# deployment repo (yuanw/Qwen3.8-Flash-Next-Single-DGX-Spark-TensorFold),
# where the image build applies it to the pip-installed site-packages
# with `patch -p0` (its hunk headers are relative to site-packages, e.g.
# tensorfold/cuda/geometry.py). Here postInstall replays exactly that:
# patch -p0 inside $out's site-packages. (patches/languages/ is only for
# the DRAFT_LANGUAGE image and is deliberately not applied.)
#
# pyproject.toml deliberately does not declare torch/triton ("CUDA uses
# the container's torch and triton"), so they are pinned here explicitly
# — the caller's python313Packages scope must carry the CUDA-enabled
# torch (nixpkgs.config.cudaSupport + cudaCapabilities on dgx-spark).
{
  lib,
  python,
  buildPythonPackage,
  fetchFromGitHub,
  setuptools,
  torch,
  triton,
  numpy,
  huggingface-hub,
  tokenizers,
  safetensors,
  jinja2,
  # --vision extras (the deployment repo pins transformers 5.17.0, which
  # is what python313Packages ships)
  pillow,
  av,
  transformers,
}:

let
  version = "0.6.1";

  inherit (import ./src.nix { inherit fetchFromGitHub; }) src deploySrc;
in
buildPythonPackage {
  pname = "tensorfold";
  inherit version;
  pyproject = true;

  inherit src;

  build-system = [ setuptools ];

  dependencies = [
    torch
    triton
    numpy
    huggingface-hub
    tokenizers
    safetensors
    jinja2
    pillow
    av
    transformers
  ];

  # The image build's `RUN patch -p0` over site-packages, replayed on
  # $out. Glob order is lexical, i.e. the 0001..0009 recipe order.
  postInstall = ''
    for patchFile in ${deploySrc}/patches/*.patch; do
      echo "applying $(basename "$patchFile")"
      patch -p0 -d "$out/${python.sitePackages}" < "$patchFile"
    done
  '';

  doCheck = false;
  pythonImportsCheck = [ ]; # importing tensorfold.cuda pulls in torch+GPU plumbing

  meta = with lib; {
    description = "Fast, exact LLM decoding on NVIDIA GPUs behind an OpenAI-compatible endpoint (CUDA JIT packaging for DGX Spark)";
    homepage = "https://github.com/ashhart/TensorFold";
    changelog = "https://github.com/ashhart/TensorFold/blob/v${version}/CHANGELOG.md";
    license = licenses.asl20;
    mainProgram = "tensorfold";
    platforms = platforms.linux;
  };
}
