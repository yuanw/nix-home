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

    # Wrapper for transcribe CLI
    makeWrapper $out/lib/transcribe $out/bin/cohere-transcribe \
      --run '
        MODEL_DIR="''${COHERE_TRANSCRIBE_MODEL:-''${XDG_DATA_HOME:-$HOME/.local/share}/cohere-transcribe/cohere-transcribe-03-2026}"
        if [ ! -d "$MODEL_DIR" ]; then
          echo "Error: Model not found at $MODEL_DIR" >&2
          echo "" >&2
          echo "To download the model (requires HuggingFace account + license acceptance):" >&2
          echo "  1. Accept license at: https://huggingface.co/CohereLabs/cohere-transcribe-03-2026" >&2
          echo "  2. pip install huggingface_hub" >&2
          echo "  3. huggingface-cli login" >&2
          echo "  4. huggingface-cli download CohereLabs/cohere-transcribe-03-2026 --local-dir \"$MODEL_DIR\"" >&2
          echo "  5. cp '"$out"'/share/cohere-transcribe/vocab.json \"$MODEL_DIR/\"" >&2
          echo "" >&2
          echo "Or set COHERE_TRANSCRIBE_MODEL to your model directory." >&2
          exit 1
        fi
        if [ ! -f "$MODEL_DIR/vocab.json" ]; then
          echo "Copying vocab.json to model directory..." >&2
          cp '"$out"'/share/cohere-transcribe/vocab.json "$MODEL_DIR/"
        fi
      ' \
      --add-flags '--model-dir "$MODEL_DIR"'

    # Wrapper for server
    makeWrapper $out/lib/transcribe-server $out/bin/cohere-transcribe-server \
      --run '
        MODEL_DIR="''${COHERE_TRANSCRIBE_MODEL:-''${XDG_DATA_HOME:-$HOME/.local/share}/cohere-transcribe/cohere-transcribe-03-2026}"
        if [ ! -d "$MODEL_DIR" ]; then
          echo "Error: Model not found. Run cohere-transcribe for setup instructions." >&2
          exit 1
        fi
        if [ ! -f "$MODEL_DIR/vocab.json" ]; then
          cp '"$out"'/share/cohere-transcribe/vocab.json "$MODEL_DIR/"
        fi
      ' \
      --add-flags '--model-dir "$MODEL_DIR"'

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
