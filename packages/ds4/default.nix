{
  lib,
  fetchFromGitHub,
  cudaPackages,
  writeShellScriptBin,
}:

let
  # Wrapper for download_model.sh that defaults to the current working
  # directory instead of the Nix store path (which is read-only).
  downloadModel = writeShellScriptBin "ds4-download-model" ''
    : "${""}{DS4_GGUF_DIR:=$PWD/gguf}
    export DS4_GGUF_DIR

    # Find the original download_model.sh shipped with ds4
    script="${placeholder "out"}/share/ds4/download_model.sh"

    exec "$script" "$@"
  '';
in
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

  buildPhase = ''
    runHook preBuild
    make ds4 ds4-server ds4-bench ds4-eval \
      NVCC=nvcc \
      NATIVE_CPU_FLAG= \
      "NVCCFLAGS=-O3 --use_fast_math -arch=sm_100 -Xcompiler -pthread" \
      "CUDA_LDLIBS=-lm -Xcompiler -pthread -lcudart -lcublas"
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin $out/share/ds4
    cp ds4 ds4-server ds4-bench ds4-eval $out/bin/
    cp download_model.sh $out/share/ds4/
    cp ${downloadModel}/bin/ds4-download-model $out/bin/
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
