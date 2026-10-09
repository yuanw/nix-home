# tensorfold-native — TensorFold's Zig engine (branch zig-flashnext) on CUDA,
# built natively, no container. The recipe's Dockerfile does the same:
# apply patches/*.patch to TensorFold, `zig build … fatbins install native`
# with nvcc, then capture the Triton cubins with tools/zig/flashnext_aot.py.
#
# Deviations from the container build (both required by nixpkgs):
#  - cuda_nvcc ships no cuda_runtime.h, so nvcc is wrapped with
#    -I<cudaHome>/include (cudaHome merges nvcc + cudart + cccl + …).
#  - the AOT spec (kernels.json) records the container's source_root and
#    triton libdevice path; both are rewritten to this build's paths. The
#    resulting cubins are valid but not byte-identical to the container's
#    capture — see docs/tensorfold-zig-plan.org.
#
# STATUS: first cut, not yet `nix build`-validated. See the plan for the
# validated spike (fatbins + native binary + 320-kernel AOT all built by hand).
{
  lib,
  stdenv,
  fetchFromGitHub,
  fetchurl,
  symlinkJoin,
  xz,
  git,
  cudaPackages,
  python313Packages,
}:

let
  inherit (import ./src.nix { inherit fetchFromGitHub fetchurl; })
    src
    recipeSrc
    zigSrc
    ;

  # The repo's pinned nixpkgs tops out at zig_0_16; ship the 0.17.0 tarball.
  zig = stdenv.mkDerivation {
    pname = "zig";
    version = "0.17.0";
    src = zigSrc;
    nativeBuildInputs = [ xz ];
    dontConfigure = true;
    dontBuild = true;
    installPhase = ''
      mkdir -p "$out"
      cp -r . "$out/"
    '';
  };

  inherit (cudaPackages)
    cuda_nvcc
    cuda_crt
    cuda_cudart
    cuda_cccl
    cuda_nvrtc
    cuda_nvtx
    libcublas
    libcurand
    libcusparse
    libcusolver
    ;

  cudaHome = symlinkJoin {
    name = "cuda-merged-${cudaPackages.cudaMajorMinorVersion}";
    paths = [
      cuda_nvcc
      cuda_crt # include/crt/host_config.h, which cuda_runtime.h needs
      cuda_cudart
      cuda_cccl
      cuda_nvrtc
      cuda_nvtx
      libcublas
      libcurand
      libcusparse
      libcusolver
    ];
  };

  python = python313Packages.python.withPackages (ps: [
    ps.torch
    ps.triton
  ]);
in
stdenv.mkDerivation (_finalAttrs: {
  pname = "tensorfold-zig";
  version = "0.6.5";

  inherit src;

  nativeBuildInputs = [
    git
    xz
  ];

  postPatch = ''
    git init -q .
    for p in ${recipeSrc}/patches/*.patch; do
      echo "applying $(basename "$p")"
      git apply "$p"
    done
  '';

  buildPhase = ''
    runHook preBuild

    export ZIG_GLOBAL_CACHE_DIR="$TMPDIR/zig-cache"
    export ZIG_LOCAL_CACHE_DIR="$TMPDIR/zig-cache"

    # nvcc needs the CUDA runtime/CCCL headers; cuda_nvcc alone has none.
    cat > "$TMPDIR/nvcc" <<EOF
    #!${stdenv.shell}
    exec ${cudaHome}/bin/nvcc -I${cudaHome}/include "\$@"
    EOF
    chmod +x "$TMPDIR/nvcc"

    ${zig}/zig build \
      -Dnvcc="$TMPDIR/nvcc" -Dsm=121 -Doptimize=fast \
      -j"$NIX_BUILD_CORES" fatbins install native

    # The spec hardcodes the container's source_root and libdevice path;
    # point both at this build, then capture the cubins.
    ${python}/bin/python - "$PWD" <<'PYEOF'
    import json, os, sys

    root = sys.argv[1] + "/src"
    import triton

    libdev = os.path.join(os.path.dirname(triton.__file__), "backends/nvidia/lib/libdevice.10.bc")
    p = "zig/tests/cuda/flashnext/kernels.json"
    d = json.load(open(p))
    d["source_root"] = root
    s = json.dumps(d).replace(
        "/usr/local/lib/python3.12/dist-packages/triton/backends/nvidia/lib/libdevice.10.bc",
        libdev,
    )
    json.dump(json.loads(s), open(p, "w"), indent=1)
    PYEOF

    PYTHONPATH="$PWD/src" ${python}/bin/python -B tools/zig/flashnext_aot.py build \
      --spec zig/tests/cuda/flashnext/kernels.json \
      --jit zig/tests/cuda/flashnext/jit.json \
      --tp 1 --out "$TMPDIR/sm121"

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p "$out/bin" "$out/share/tensorfold/cuda"
    install -Dm755 zig-out/native/bin/tensorfold-native "$out/bin/tensorfold-native"
    cp -r zig-out/fatbin "$out/share/tensorfold/fatbin"
    cp -r "$TMPDIR/sm121" "$out/share/tensorfold/cuda/sm121"
    runHook postInstall
  '';

  meta = {
    description = "TensorFold's Zig CUDA engine (tensorfold-native) for DGX Spark";
    homepage = "https://github.com/ashhart/TensorFold";
    license = lib.licenses.asl20;
    platforms = [ "aarch64-linux" ];
  };
})
