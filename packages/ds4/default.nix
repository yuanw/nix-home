{
  lib,
  fetchFromGitHub,
  cudaPackages,
}:

cudaPackages.backendStdenv.mkDerivation {
  pname = "ds4";
  version = "0-unstable-2026-06-17";

  src = fetchFromGitHub {
    owner = "antirez";
    repo = "ds4";
    rev = "80ebbc396aee40eedc1d829222f3362d10fa4c6c";
    hash = "sha256-Ieuc72GHZs20ModQfnvI5Me31n4Pj+WFYtsuqaKJceo=";
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

    # Patch the script so the ds4flash.gguf symlink is created in the
    # caller's working directory instead of the read-only Nix store.
    substituteInPlace $out/share/ds4/download_model.sh \
      --replace 'cd "$ROOT"' '# cd "$ROOT"  # patched by Nix: use $PWD instead'

    cat >$out/bin/ds4-download-model <<'EOF'
    #!/bin/sh
    : "''${DS4_GGUF_DIR:=$PWD/gguf}"
    export DS4_GGUF_DIR
    script="PLACEHOLDER"
    exec "$script" "$@"
    EOF
    substituteInPlace $out/bin/ds4-download-model --replace PLACEHOLDER "$out/share/ds4/download_model.sh"
    chmod +x $out/bin/ds4-download-model

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
