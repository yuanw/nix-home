# vllm-node — reproducible Docker image for DGX Spark (aarch64-linux, sm_121a)
#
# Builds the spark-vllm-docker runner image using prebuilt wheels pinned by
# hash. All inputs (source commit, wheel hashes) are pinned for reproducibility.
#
# Source: https://github.com/eugr/spark-vllm-docker
# Benchmark: https://spark-arena.com/benchmark/sub1782724431960 (109 tok/s)

{
  lib,
  stdenv,
  fetchFromGitHub,
  fetchurl,
}:

let
  # Pinned spark-vllm-docker source (Dockerfile + build script)
  src = fetchFromGitHub {
    owner = "eugr";
    repo = "spark-vllm-docker";
    rev = "5c9bee26c24016d80b340aa8477304f743f441de";
    hash = "sha256-8nixMpafY9y4quZTEWCECcOtziLYzpNmAF1viuQb+DM=";
  };

  # Prebuilt wheels — pinned by hash for reproducibility
  wheelSrcs = [
    (fetchurl {
      name = "vllm-0.23.1rc1.dev701+g00eb7cefa.d20260701-cp312-cp312-linux_aarch64.whl";
      url = "https://github.com/eugr/spark-vllm-docker/releases/download/prebuilt-vllm-current/vllm-0.23.1rc1.dev701%2Bg00eb7cefa.d20260701-cp312-cp312-linux_aarch64.whl";
      hash = "sha256-KIBU98ySKRBLGuGCglAqDM1SR+cNMtCr3Z34DLpIUSs=";
    })
    (fetchurl {
      name = "flashinfer_cubin-0.6.14-py3-none-any.whl";
      url = "https://github.com/eugr/spark-vllm-docker/releases/download/prebuilt-flashinfer-current/flashinfer_cubin-0.6.14-py3-none-any.whl";
      hash = "sha256-X2uqTkPU8j7fN2WGfxmrHRm83sUy5GzzWj72HNg1pN4=";
    })
    (fetchurl {
      name = "flashinfer_jit_cache-0.6.14-cp39-abi3-manylinux_2_28_aarch64.whl";
      url = "https://github.com/eugr/spark-vllm-docker/releases/download/prebuilt-flashinfer-current/flashinfer_jit_cache-0.6.14-cp39-abi3-manylinux_2_28_aarch64.whl";
      hash = "sha256-K8HreX7Y922Tkt2ehzbRwxly3iHbFAy1CqeD0MxAlq0=";
    })
    (fetchurl {
      name = "flashinfer_python-0.6.14-py3-none-any.whl";
      url = "https://github.com/eugr/spark-vllm-docker/releases/download/prebuilt-flashinfer-current/flashinfer_python-0.6.14-py3-none-any.whl";
      hash = "sha256-MaIfJnu2WH0jdaxnsKh+AUuBXBXRRheWmU7x6TqTD60=";
    })
  ];

  # Copy one wheel from store to output
  copyWheel = wheel: ''
    cp ${wheel} "$out/wheels/${wheel.name}"
  '';

in

stdenv.mkDerivation {
  name = "vllm-node-image";
  version = "0.23.1rc1.dev701";

  dontUnpack = true;

  buildPhase = ''
        mkdir -p $out
        mkdir -p $out/wheels
        mkdir -p $out/spark-vllm-docker

        # Copy pinned spark-vllm-docker source
        cp -r ${src}/* $out/spark-vllm-docker/
        chmod -R +w $out/spark-vllm-docker

        # Copy pinned wheels into output
        ${lib.concatStringsSep "\n" (map copyWheel wheelSrcs)}

        # Create reproducible build script
        cat > $out/build-image.sh << 'BUILDSCRIPT'
    #!/usr/bin/env bash
    set -euo pipefail

    SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
    SPARK_DIR="$SCRIPT_DIR/spark-vllm-docker"
    WHEELS="$SCRIPT_DIR/wheels"
    IMAGE_TAG="localhost/vllm-node:latest"

    # Use podman (preferred) or docker
    if command -v podman &>/dev/null; then
        DOCKER=podman
    elif command -v docker &>/dev/null; then
        DOCKER=docker
    else
        echo "ERROR: No container runtime (podman/docker) found"
        exit 1
    fi

    echo "Building vllm-node image ($IMAGE_TAG) from pinned inputs..."

    # Work in a temp dir so we can write wheels into the build context
    BUILD_DIR="$(mktemp -d)"
    trap 'rm -rf "$BUILD_DIR"' EXIT

    # Copy spark-vllm-docker source to writable temp dir
    cp -r "$SPARK_DIR"/* "$BUILD_DIR/"
    chmod -R +w "$BUILD_DIR/wheels"

    # Copy pinned wheels into build context
    cp "$WHEELS"/*.whl "$BUILD_DIR/wheels/"

    # Generate build-metadata.yaml (expected by Dockerfile)
    cat > "$BUILD_DIR/build-metadata.yaml" << YAMLEOF
    build_date: $(date -u +%Y-%m-%dT%H:%M:%SZ)
    build_script_commit: "reproducible-nix-build"
    gpu_arch: "12.1a"
    YAMLEOF

    # Build runner image — base CUDA image is pulled as needed
    cd "$BUILD_DIR"
    $DOCKER build \
      -t "$IMAGE_TAG" \
      --build-arg BUILD_JOBS=16 \
      --build-arg TORCH_CUDA_ARCH_LIST=12.1a \
      --build-arg FLASHINFER_CUDA_ARCH_LIST=12.1a \
      .

    echo "Build complete: $IMAGE_TAG"
    BUILDSCRIPT

        chmod +x $out/build-image.sh

        # Wrapper in PATH
        mkdir -p $out/bin
        cat > $out/bin/build-vllm-node << 'BINWRAP'
    #!/usr/bin/env bash
    exec "$(cd "$(dirname "$0")/.." && pwd)/build-image.sh"
    BINWRAP
        chmod +x $out/bin/build-vllm-node
  '';

  meta = with lib; {
    description = "Reproducible vllm-node Docker image for DGX Spark";
    longDescription = ''
      Pinned build artifacts for the spark-vllm-docker runner image.
      Run 'build-vllm-node' to build the Docker image from pinned inputs.
      When the source commit, wheel hashes, or base image change, update
      the pinned values here and rebuild.
    '';
    homepage = "https://github.com/eugr/spark-vllm-docker";
    license = licenses.asl20;
    platforms = [ "aarch64-linux" ];
  };
}
