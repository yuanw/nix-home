{
  lib,
  stdenv,
  fetchzip,
  makeWrapper,
}:

stdenv.mkDerivation rec {
  pname = "cohere-transcribe";
  version = "0.1.1";

  src = fetchzip {
    url = "https://github.com/second-state/cohere_transcribe_rs/releases/download/v${version}/transcribe-macos-aarch64.zip";
    hash = "sha256-JX7V+6AFnwfDsWjwpEr8+T94h6hNI5TTckmX+nDilFk=";
  };

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/lib $out/share/cohere-transcribe

    # Copy binaries and libraries
    cp $src/transcribe $out/lib/
    cp $src/transcribe-server $out/lib/
    cp -r $src/mlx.metallib $out/lib/ 2>/dev/null || true
    cp $src/vocab.json $out/share/cohere-transcribe/


    runHook postInstall
  '';

  meta = {
    description = "Cohere Transcribe speech-to-text in Rust (MLX backend)";
    homepage = "https://github.com/second-state/cohere_transcribe_rs";
    license = lib.licenses.asl20;
    platforms = [ "aarch64-darwin" ];
    mainProgram = "cohere-transcribe";
  };
}
