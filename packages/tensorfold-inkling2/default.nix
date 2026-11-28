# tensorfold-native built from TensorFold PR #4441 (jschmied's Inkling 2 Mini
# CUDA engine, dc2ca8a1) plus the recipe's patches 0001–0030, natively — the
# container build of MiaAI-Lab/Inkling-2-Mini-DGX-Spark-TensorFold replicated
# without Docker:
#
#   zig build -Dnvcc=… -Doptimize=fast -Dnative-fp-decode=true fatbins native
#   zig build grammar -Dxgrammar=… (libtfgrammar.so, structured output)
#   + libturbojpeg.so.0 (3.1.4.1) and libwebpdecoder.so.3 (1.6.0) beside the
#     binary, which the vision tower dlopens and refuses unless the exact
#     pinned release (it reads the version strings; PNG is built into the
#     engine).
#
# The kernel fatbins are compiled into the binary (the recipe labels the image
# tf.kernels=embedded): no Triton AOT capture and no Python helper, unlike
# packages/tensorfold-zig — the image codecs are the only runtime extras.
# Layout matches the container's /opt/tensorfold: $out/native/bin +
# $out/native/lib (the engine finds ../lib from the binary's dir).
#
# The started server serves the checkpoint folder and drafts with the DFlash2
# model (--drafter); the NixOS unit is modules/inkling2.nix.
#
# nativeFpDecode=0 builds the generic FP4/FP8 decode instead of PTX ISA 9.2's
# native cvt (sm_121a, needs nvcc 13.2+ — the Spark has it; see
# hosts/dgx-spark/cuda-fixes.nix).
{
  lib,
  stdenv,
  fetchFromGitHub,
  fetchgit,
  fetchurl,
  symlinkJoin,
  xz,
  git,
  patchelf,
  cmake,
  cudaPackages,
  nativeFpDecode ? true,
}:

let
  inherit
    (import ./src.nix {
      inherit fetchFromGitHub fetchgit fetchurl;
    })
    src
    recipeSrc
    xgrammarSrc
    jpegSrc
    webpSrc
    ;

  # The repo's pinned nixpkgs tops out at zig_0_16; ship the 0.17.0 tarball
  # (same fetch packages/tensorfold-zig uses — shared through its src.nix).
  zigSrc =
    (import ../tensorfold-zig/src.nix {
      inherit fetchFromGitHub fetchurl;
    }).zigSrc;
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

  # The engine reads an exact version string out of these binaries and refuses
  # any other release, so they are built from the recipe's pinned tarballs,
  # not nixpkgs' (libjpeg-turbo 3.1.4.1, libwebp 1.6.0).
  # cmake in nativeBuildInputs would activate stdenv's cmake hook; the hook and
  # this manual -S . -B build fight (the hook's second configure fails), so the
  # hooks are off and both codecs configure/build/install themselves.
  jpegCodec = stdenv.mkDerivation {
    pname = "libturbojpeg-pinned";
    version = "3.1.4.1";
    src = jpegSrc;
    dontUseCmakeConfigure = true;
    dontUseCmakeBuild = true;
    dontUseCmakeInstall = true;
    nativeBuildInputs = [ cmake ];
    buildPhase = ''
      cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DENABLE_SHARED=ON -DENABLE_STATIC=OFF -DWITH_TURBOJPEG=ON
      cmake --build build --target turbojpeg -j"$NIX_BUILD_CORES"
    '';
    installPhase = ''
      mkdir -p "$out"
      cp -a build/libturbojpeg.so.0* "$out/"
    '';
  };

  webpCodec = stdenv.mkDerivation {
    pname = "libwebpdecoder-pinned";
    version = "1.6.0";
    src = webpSrc;
    dontUseCmakeConfigure = true;
    dontUseCmakeBuild = true;
    dontUseCmakeInstall = true;
    nativeBuildInputs = [ cmake ];
    buildPhase = ''
      cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON \
        -DWEBP_BUILD_ANIM_UTILS=OFF -DWEBP_BUILD_CWEBP=OFF -DWEBP_BUILD_DWEBP=OFF \
        -DWEBP_BUILD_GIF2WEBP=OFF -DWEBP_BUILD_IMG2WEBP=OFF -DWEBP_BUILD_VWEBP=OFF \
        -DWEBP_BUILD_WEBPINFO=OFF -DWEBP_BUILD_WEBPMUX=OFF -DWEBP_BUILD_EXTRAS=OFF \
        -DWEBP_BUILD_LIBWEBPMUX=OFF
      cmake --build build --target webpdecoder -j"$NIX_BUILD_CORES"
    '';
    installPhase = ''
      mkdir -p "$out"
      cp -a build/libwebpdecoder.so.3* "$out/"
    '';
  };
in
stdenv.mkDerivation (_finalAttrs: {
  pname = "tensorfold-inkling2";
  version = "0.6.5";

  inherit src;

  nativeBuildInputs = [
    git
    xz
    patchelf
  ];

  postPatch = ''
    git init -q .
    for p in ${recipeSrc}/patches/*.patch; do
      echo "applying $(basename "$p")"
      git apply "$p"
    done
    # `zig build grammar` demands the xgrammar checkout sit at its pinned tag
    # (git describe --tags --exact-match). cp -r keeps the store path's 0555
    # mode, so chmod the copy writable before git can init it, and name its
    # commit v0.2.8.
    cp -r ${xgrammarSrc} "$TMPDIR/xgrammar"
    chmod -R u+w "$TMPDIR/xgrammar"
    (cd "$TMPDIR/xgrammar" && git init -q . && git add -A && \
     git -c user.email=build@localhost -c user.name=build commit -qm xgrammar && \
     git tag v0.2.8)
  '';

  buildPhase = ''
    runHook preBuild

    export ZIG_GLOBAL_CACHE_DIR="$TMPDIR/zig-cache"
    export ZIG_LOCAL_CACHE_DIR="$TMPDIR/zig-cache"
    export HOME="$TMPDIR"
    export XDG_CACHE_HOME="$TMPDIR/.cache"
    export CUDA_CACHE_PATH="$TMPDIR/cuda-cache"

    # nvcc needs the CUDA runtime/CCCL headers; cuda_nvcc alone has none.
    cat > "$TMPDIR/nvcc" <<EOF
    #!${stdenv.shell}
    exec ${cudaHome}/bin/nvcc -I${cudaHome}/include "\$@"
    EOF
    chmod +x "$TMPDIR/nvcc"

    # Kernels are embedded in tensorfold-native (the fatbins are intermediates;
    # the fatbin/ install copies land under $out but the server never reads
    # them). One rank, sm_121.
    ${zig}/zig build \
      -Dnvcc="$TMPDIR/nvcc" -Doptimize=fast -Dnative-fp-decode=${
        if nativeFpDecode then "true" else "false"
      } \
      -j"$NIX_BUILD_CORES" fatbins native --prefix "$out"

    # libtfgrammar.so (structured output, required tool calls) from the patched
    # tree's tf_grammar.cc over xgrammar v0.2.8 (patch 0013 turns xgrammar's own
    # compile cache off — a v0.2.8 bug). The sandbox has no glibc to detect, so
    # zig's native target resolves to musl and libc++ then fails on musl's
    # bits/alltypes.h; the container build detected glibc instead — -gnu forces
    # zig's bundled glibc headers (the engine's own build stays `native`, whose
    # binary is glibc-NEEDED anyway; postFixup sets its loader).
    ${zig}/zig build grammar -Dxgrammar="$TMPDIR/xgrammar" -Dtarget=aarch64-linux-gnu \
      -Doptimize=ReleaseFast -j"$NIX_BUILD_CORES" --prefix "$out"

    test -x "$out/native/bin/tensorfold-native"
    test -s "$out/native/lib/libtfgrammar.so"

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    # The codecs sit in the binary's ../lib, where the vision tower dlopens
    # them (TENSORFOLD_LIBTURBOJPEG / TENSORFOLD_LIBWEBP override the path).
    mkdir -p "$out/native/lib"
    cp -a ${jpegCodec}/libturbojpeg.so.0* "$out/native/lib/"
    cp -a ${webpCodec}/libwebpdecoder.so.3* "$out/native/lib/"
    # A plain name for ad-hoc runs; the NixOS unit uses the native/ path.
    mkdir -p "$out/bin"
    ln -s "$out/native/bin/tensorfold-native" "$out/bin/tensorfold-native"
    runHook postInstall
  '';

  # zig picks the musl interpreter for a native link (and there is no
  # /lib/ld-musl-aarch64.so.1 on NixOS); the binary actually needs glibc
  # (NEEDED libc.so.6), so point it at the Nix glibc loader and bake the
  # glibc + CUDA rpath (cuBLASLt etc. are dlopened, so the unit also sets
  # LD_LIBRARY_PATH).
  postFixup = ''
    patchelf \
      --set-interpreter ${stdenv.cc.libc}/lib/ld-linux-aarch64.so.1 \
      --set-rpath "${stdenv.cc.libc}/lib:${cudaHome}/lib64:${cudaHome}/lib" \
      "$out/native/bin/tensorfold-native"
  '';

  meta = {
    description = "TensorFold's Zig CUDA engine (tensorfold-native) serving Inkling 2 Mini on a DGX Spark";
    homepage = "https://github.com/ashhart/TensorFold";
    license = lib.licenses.asl20;
    platforms = [ "aarch64-linux" ];
  };
})
