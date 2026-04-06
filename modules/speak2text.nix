{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.speak2text;

  pythonWithHf = pkgs.python3.withPackages (p: [ p.huggingface-hub ]);

  parakeet-mlx-init = pkgs.writeShellApplication {
    name = "parakeet-mlx-init";
    runtimeInputs = [ pythonWithHf ];
    text = ''
      echo "Downloading mlx-community/parakeet-tdt-0.6b-v3 to HuggingFace cache..." >&2
      python3 -c "
      from huggingface_hub import snapshot_download
      path = snapshot_download('mlx-community/parakeet-tdt-0.6b-v3')
      print(f'Model cached at: {path}')
      "
    '';
  };

  cohere-transcribe-init = pkgs.writeShellApplication {
    name = "cohere-transcribe-init";
    runtimeInputs = [ pythonWithHf ];
    text = ''
      MODEL_DIR="''${COHERE_TRANSCRIBE_MODEL_DIR:-''${HOME}/.local/share/cohere-transcribe/models/cohere-transcribe-03-2026}"
      echo "Note: CohereLabs/cohere-transcribe-03-2026 is an access-controlled model." >&2
      echo "Run 'huggingface-cli login' first if you haven't already." >&2
      echo "Downloading to ''${MODEL_DIR} ..." >&2
      huggingface-cli download CohereLabs/cohere-transcribe-03-2026 \
        --local-dir "''${MODEL_DIR}"
      echo "Copying vocab.json from Nix package..." >&2
      cp ${pkgs.cohere-transcribe}/share/cohere-transcribe/vocab.json "''${MODEL_DIR}/"
      echo "Done. Model ready at ''${MODEL_DIR}" >&2
    '';
  };

  # Shared recording boilerplate from claude-voice.nix pattern.
  # transcribeExpr is a shell snippet that sets TRANSCRIPT from $TMPFILE.
  mkSpeak2Text =
    {
      transcribeExpr,
      runtimeInputs,
    }:
    pkgs.writeShellApplication {
      name = "speak2text";
      runtimeInputs = [
        pkgs.sox
        pkgs.ffmpeg
      ]
      ++ runtimeInputs;
      text = ''
        TMPFILE=$(mktemp /tmp/speak2text-XXXXXX.wav)
        trap 'rm -f "$TMPFILE"' EXIT

        step() { printf '[%s] %s\n' "$(date +%H:%M:%S)" "$*" >&2; }

        step 'Press Enter to start recording...'
        read -r _

        sox -d -r 16000 -c 1 -b 16 "$TMPFILE" 2>/dev/null &
        RECORD_PID=$!
        sleep 0.5

        if ! kill -0 "$RECORD_PID" 2>/dev/null; then
          step 'sox unavailable, falling back to ffmpeg'
          ffmpeg -f avfoundation -i ":0" \
            -ar 16000 -ac 1 -acodec pcm_s16le \
            -y -loglevel error "$TMPFILE" &
          RECORD_PID=$!
        fi

        step 'Recording... press Enter to stop.'
        read -r _
        kill -INT "$RECORD_PID" 2>/dev/null
        wait "$RECORD_PID" 2>/dev/null || true

        if [[ ! -s "$TMPFILE" ]]; then
          step 'No audio captured'
          exit 1
        fi

        step "Recorded $(wc -c < "$TMPFILE") bytes — transcribing..."

        TRANSCRIPT=""
        ${transcribeExpr}

        TRANSCRIPT=$(printf '%s' "$TRANSCRIPT" | tr -d '\n' | sed 's/^[[:space:]]*//')

        if [[ -z "$TRANSCRIPT" ]]; then
          step 'No speech detected'
          exit 1
        fi

        printf '%s' "$TRANSCRIPT" | pbcopy
        step "Copied to clipboard: $TRANSCRIPT"
      '';
    };

  speak2text-parakeet = mkSpeak2Text {
    runtimeInputs = [ pkgs.parakeet-mlx ];
    transcribeExpr = ''
      REAL_TMPFILE=$(realpath "$TMPFILE")
      TXT_OUT="''${REAL_TMPFILE%.wav}.txt"
      ( cd "$(dirname "$REAL_TMPFILE")" && parakeet-mlx --output-format txt "$REAL_TMPFILE" ) \
        && PARAKEET_EXIT=0 || PARAKEET_EXIT=$?
      if [[ $PARAKEET_EXIT -ne 0 ]]; then
        step "parakeet-mlx exited with code $PARAKEET_EXIT"
      fi
      TRANSCRIPT=$(cat "$TXT_OUT" 2>/dev/null || true)
      rm -f "$TXT_OUT"
    '';
  };

  speak2text-cohere = mkSpeak2Text {
    runtimeInputs = [ ];
    transcribeExpr = ''
      MODEL_DIR="''${COHERE_TRANSCRIBE_MODEL_DIR:-''${HOME}/.local/share/cohere-transcribe/models/cohere-transcribe-03-2026}"
      TRANSCRIPT=$(${pkgs.cohere-transcribe}/lib/transcribe --model-dir "''${MODEL_DIR}" "$TMPFILE" 2>/dev/null || true)
    '';
  };

  speak2text-whispercpp = mkSpeak2Text {
    runtimeInputs = [ pkgs.whisper-cpp ];
    transcribeExpr = ''
      if [[ -z "''${WHISPER_MODEL:-}" ]]; then
        step 'Set $WHISPER_MODEL to the path of a ggml model file (e.g. ggml-base.en.bin)'
        exit 1
      fi
      TRANSCRIPT=$(whisper-cpp -m "''${WHISPER_MODEL}" -f "$TMPFILE" -nt 2>/dev/null || true)
    '';
  };

  packages = {
    whispercpp = [
      pkgs.whisper-cpp
      speak2text-whispercpp
    ];
    parakeet-mlx = [
      pkgs.parakeet-mlx
      pkgs.parakeet-transcribe
      parakeet-mlx-init
      speak2text-parakeet
    ];
    cohere-transcribe = [
      pkgs.cohere-transcribe
      cohere-transcribe-init
      speak2text-cohere
    ];
  };
in
{
  options.modules.speak2text = {
    enable = lib.mkEnableOption "speak2text";
    flavor = lib.mkOption {
      type = lib.types.enum [
        "whispercpp"
        "parakeet-mlx"
        "cohere-transcribe"
      ];
      default = "parakeet-mlx";
      description = "Speech-to-text backend. whispercpp: cross-platform CPU; parakeet-mlx: Apple Silicon MLX; cohere-transcribe: Rust + MLX (aarch64-darwin only).";
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = packages.${cfg.flavor};
    };
  };
}
