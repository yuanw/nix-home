# Prebuilt Rust ASR CLI from https://github.com/second-state/cohere_transcribe_rs,
# one zip per platform.  Each zip holds one directory with the two binaries, the
# vocabulary, and the libraries that the binaries load at run time:
#
#     transcribe-macos-aarch64/       transcribe  transcribe-server  vocab.json
#                                     mlx.metallib
#     transcribe-linux-aarch64-cuda/  transcribe  transcribe-server  vocab.json
#                                     libtorch/lib/*.so
#
# fetchzip strips that one directory (`stripRoot` defaults to true), so $src
# below is flat, and the libraries are installed beside the binary that looks
# for them — which is what the vendor's link step expects (RPATH =
# $ORIGIN/libtorch/lib).

{
  lib,
  stdenv,
  fetchzip,
  makeWrapper,
  autoPatchelfHook,
  # callPackage fills this in, so what arrives here is whatever `cudaPackages`
  # happens to be in the package set this package is built from — not
  # necessarily a CUDA 12 set, and on aarch64-darwin not necessarily a CUDA set
  # at all.  Hence the checks on it below instead of `with cudaPackages`.
  cudaPackages,
  ...
}:

let
  isLinux = stdenv.hostPlatform.isLinux;

  # CUDA keeps the major version in the soname.  What the Linux binary asks the
  # loader for is libcudart.so.12, libcublas.so.12, libcublasLt.so.12,
  # libcusparse.so.12, libcufft.so.11, libcurand.so.10, libcudnn.so.9,
  # libcufile.so.0 and libnvrtc.so.12, so a CUDA 13 package set — which has
  # .so.13 versions of some of those and nothing at all of the rest — cannot
  # stand in for a CUDA 12 one.
  #
  # Which set the name `cudaPackages` designates is not fixed either: nixpkgs
  # currently has it at CUDA 12.9, while hosts/dgx-spark/cuda-fixes.nix
  # re-binds it to cudaPackages_13_2.  Hence the version test, and the warning
  # when there is nothing to test: a set with no `cuda_cudart` is a package set
  # with CUDA turned off (which is what aarch64-darwin has), and an empty
  # LD_LIBRARY_PATH/RPATH is how that shows up later.
  cuda12 =
    isLinux
    && cudaPackages ? cuda_cudart
    && lib.hasPrefix "12." (cudaPackages.cudaMajorMinorVersion or "");

  cudaLibs = with cudaPackages; [
    cuda_cudart
    cuda_nvrtc
    libcublas
    libcusparse
    libcufft
    libcurand
    cudnn
    libcufile
  ];

  # On Linux: the CUDA runtime libraries the shipped libtorch loads but does not
  # bundle, plus libstdc++/libgcc.  Empty on darwin, where nothing of this kind
  # is wanted (mlx.metallib is copied next to the binary and needs no loader
  # path).
  cudaRuntimeLibs =
    if !isLinux then
      [ ]
    else if !cuda12 then
      builtins.trace ''
        cohere-transcribe: no CUDA 12 libraries in sight (this package set has
        ${cudaPackages.cudaMajorMinorVersion or "no CUDA at all"}), so the Linux
        build will not run: transcribe-linux-aarch64-cuda.zip asks the loader for
        libcudart.so.12.  Feed it a CUDA 12 cudaPackages — nixpkgs has
        cudaPackages_12_6 up to _12_9 — or keep the package off the host.
      '' [ ]
    else
      cudaLibs;

  # Directories the loader has to be told about, because $ORIGIN/libtorch/lib
  # and RPATH do not cover them: the two inside this output ($out/lib for a call
  # that comes in through the $out/bin symlink, where $ORIGIN would otherwise be
  # $out/bin and empty, and $out/lib/libtorch/lib for the copied libraries), the
  # C++ runtime, and the driver directory (/run/opengl-driver/lib — libcuda.so.1
  # is the driver, which NixOS keeps there).
  libraryPath = lib.concatStringsSep ":" (
    [
      "$out/lib"
      "$out/lib/libtorch/lib"
      "${lib.getLib stdenv.cc.cc}/lib"
      "/run/opengl-driver/lib"
    ]
    ++ lib.optionals cuda12 [ (lib.makeLibraryPath cudaLibs) ]
  );

  sources = {
    aarch64-darwin = {
      url = "transcribe-macos-aarch64.zip";
      hash = "sha256-JX7V+6AFnwfDsWjwpEr8+T94h6hNI5TTckmX+nDilFk=";
    };
    aarch64-linux = {
      url = "transcribe-linux-aarch64-cuda.zip";
      hash = "sha256-rBahlKp/MaLSm0NvcWkq94OeF7Na0dt6zE525MO0TqI=";
    };
  };

  source =
    sources.${stdenv.hostPlatform.system}
      or (throw "cohere-transcribe: upstream ships no binary for ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation rec {
  pname = "cohere-transcribe";
  version = "0.1.1";

  src = fetchzip {
    url = "https://github.com/second-state/cohere_transcribe_rs/releases/download/v${version}/${source.url}";
    hash = source.hash;
  };

  nativeBuildInputs = [ makeWrapper ] ++ lib.optionals isLinux [ autoPatchelfHook ];

  buildInputs = lib.optionals isLinux ([ stdenv.cc.cc.lib ] ++ cudaRuntimeLibs);

  # Let libcuda.so.1 through: autoPatchelfHook cannot supply a driver, only
  # look for one.
  autoPatchelfIgnoreMissingDeps = [ "libcuda.so.1" ];

  # The layout that modules/speech2text/* already calls the CLI through: real
  # binaries in $out/lib, with the libraries beside them, and $out/bin holding
  # only symlinks.
  #
  # The symlinks exist so that `cohere-transcribe` is on PATH, and are named
  # that way because a bare `transcribe` would collide with pkgs.transcribe, the
  # pipeline script that calls this CLI.  A *wrapper* in $out/bin instead of a
  # symlink would have put the script's own directory in $ORIGIN — with nothing
  # in it — and taken the $ORIGIN-relative rpath away from the real binary.
  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib $out/bin $out/share/cohere-transcribe

    for f in transcribe transcribe-server; do
      cp $src/$f $out/lib/$f
      chmod +x $out/lib/$f
      ln -s $out/lib/$f $out/bin/cohere-$f
    done

    if [ -d $src/libtorch ]; then cp -r $src/libtorch $out/lib/libtorch; fi
    if [ -f $src/mlx.metallib ]; then cp $src/mlx.metallib $out/lib/; fi
    cp $src/vocab.json $out/share/cohere-transcribe/

    runHook postInstall
  '';

  # The wrapper adds LD_LIBRARY_PATH; the RPATH that the vendor built with is
  # left in place (autoPatchelfHook adds to it in preFixup, which runs before
  # this phase).  What neither of those two reaches is the driver directory
  # under /run, and $out/lib itself for a call that comes in through $out/bin:
  # see libraryPath above.
  #
  # Nothing of this applies on darwin, which has no loader that reads
  # LD_LIBRARY_PATH.
  postFixup = lib.optionalString isLinux ''
    for f in transcribe transcribe-server; do
      wrapProgram $out/lib/$f \
        --prefix LD_LIBRARY_PATH : "${libraryPath}"
    done
  '';

  meta = {
    description = "Cohere Transcribe speech-to-text in Rust (MLX on darwin, libtorch + CUDA on linux)";
    homepage = "https://github.com/second-state/cohere_transcribe_rs";
    license = lib.licenses.asl20;
    mainProgram = "cohere-transcribe";
    platforms = [
      "aarch64-darwin"
      "aarch64-linux"
    ];
  };
}
