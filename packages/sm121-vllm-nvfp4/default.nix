# sm121-vllm-nvfp4 — SM121 vLLM v0.24.0 NVFP4 KV Docker image
#
# Builds the NVIDIA-optimized vLLM runtime for DGX Spark (aarch64-linux,
# sm_121a) with NVFP4 KV cache support, MTP speculative decoding, and
# FlashInfer PR #3684 + vLLM PR #46329 patches.
#
# Source: https://github.com/r0b0tlab/nvidia-qwen-3.6-27B-sm121-nvfp4
# Benchmark: 248 tok/s (32 concurrency), GSM8K 81.88% 0-shot

{
  lib,
  stdenv,
  fetchFromGitHub,
}:

let
  src = fetchFromGitHub {
    owner = "r0b0tlab";
    repo = "nvidia-qwen-3.6-27B-sm121-nvfp4";
    rev = "2991336643f43f95eedf163d236bff4f95ee808d";
    hash = "sha256-0wg49rf1dr05bbnsm1h20gl9jsaykaz8098pvbw4453lifsswc56";
  };

in

stdenv.mkDerivation {
  name = "sm121-vllm-nvfp4-image";
  version = "0.24.0";

  dontUnpack = true;

  buildPhase = ''
        mkdir -p $out

        # Copy pinned source
        cp -r ${src}/* $out/
        chmod -R +w $out

        # Create reproducible build script
        cat > $out/build-image.sh << 'BUILDSCRIPT'
    #!/usr/bin/env bash
    set -euo pipefail

    SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
    IMAGE_TAG="localhost/sm121-vllm-v0240-nvfp4:kv-exp"

    # Use podman (preferred) or docker
    if command -v podman &>/dev/null; then
        DOCKER=podman
    elif command -v docker &>/dev/null; then
        DOCKER=docker
    else
        echo "ERROR: No container runtime (podman/docker) found"
        exit 1
    fi

    echo "Building sm121-vllm-nvfp4 image ($IMAGE_TAG) from pinned source..."
    echo "NOTE: This builds vLLM v0.24.0 from source (~60 min on DGX Spark)"
    echo "       with MAX_JOBS=6 to avoid OOM."

    cd "$SCRIPT_DIR"
    $DOCKER build \
      -f docker/Dockerfile.kv-exp \
      -t "$IMAGE_TAG" \
      .

    echo "Build complete: $IMAGE_TAG"
    BUILDSCRIPT

        chmod +x $out/build-image.sh

        # Wrapper in PATH
        mkdir -p $out/bin
        cat > $out/bin/build-sm121-vllm-image << 'BINWRAP'
    #!/usr/bin/env bash
    exec "$(cd "$(dirname "$0")/.." && pwd)/build-image.sh"
    BINWRAP
        chmod +x $out/bin/build-sm121-vllm-image
  '';

  meta = with lib; {
    description = "SM121-optimized vLLM v0.24.0 Docker image with NVFP4 KV cache";
    longDescription = ''
      Docker image for DGX Spark (GB10, sm_121a) running vLLM v0.24.0
      compiled from source with:
      - NVFP4 KV cache support (FlashInfer PR #3684 + vLLM PR #46329)
      - MTP speculative decoding (1 token, 88–93% acceptance rate)
      - NVIDIA Qwen3.6-27B-NVFP4 model support

      Run 'build-sm121-vllm-image' to build the Docker image.
      Build time: ~60 minutes on DGX Spark (MAX_JOBS=6).
    '';
    homepage = "https://github.com/r0b0tlab/nvidia-qwen-3.6-27B-sm121-nvfp4";
    license = lib.licenses.asl20;
    platforms = [ "aarch64-linux" ];
  };
}
