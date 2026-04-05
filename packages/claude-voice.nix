{
  lib,
  writeShellApplication,
  sox,
  ffmpeg,
  parakeet-mlx,
  mlx-speak,
  python3Packages,
}:
let
  pythonWithHf = python3Packages.python.withPackages (p: [ p.huggingface-hub ]);
in

let
  voicePrompt = ''
    You are a voice assistant. The user is speaking to you and your response will be read aloud.
    Rules:
    - Respond in plain spoken language only — no markdown, no bullet points, no headers, no code blocks
    - Keep responses concise (1-3 sentences) unless the user explicitly asks for more detail
    - If asked for code or lists, describe them in words rather than formatting them
    - Spell out abbreviations and symbols when they appear in your response
  '';
in

writeShellApplication {
  name = "claude-voice";

  runtimeInputs = [
    sox
    ffmpeg
    parakeet-mlx
    mlx-speak
    pythonWithHf
  ];

  text = ''
    TMPFILE=$(mktemp /tmp/claude-voice-XXXXXX.wav)
    FIRST_TURN=true
    trap 'rm -f "$TMPFILE"; printf "\nGoodbye.\n" >&2' EXIT

    VOICE_PROMPT=${lib.escapeShellArg voicePrompt}

    step() { printf '[%s] %s\n' "$(date +%H:%M:%S)" "$*" >&2; }

    # -- init subcommand: download all models ---------------------------------
    if [[ "''${1:-}" == "init" ]]; then
      # STT: parakeet-mlx uses the HuggingFace cache directly
      step "Downloading STT model (mlx-community/parakeet-tdt-0.6b-v3)..."
      python3 -c "from huggingface_hub import snapshot_download; snapshot_download('mlx-community/parakeet-tdt-0.6b-v3')"
      HF_CACHE="''${HF_HOME:-''${XDG_CACHE_HOME:-$HOME/.cache}/huggingface}/hub"
      step "STT model cached at: $HF_CACHE/models--mlx-community--parakeet-tdt-0.6b-v3"

      # TTS: mlx-speech expects models at a specific path, not the HF cache
      MLX_SPEECH_MODELS="''${MLX_SPEECH_MODELS:-$HOME/.local/share/mlx-speech/models}"
      TTS_DIR="$MLX_SPEECH_MODELS/openmoss/moss_tts_local/mlx-int8"
      CODEC_DIR="$MLX_SPEECH_MODELS/openmoss/moss_audio_tokenizer/mlx-int8"
      mkdir -p "$TTS_DIR" "$CODEC_DIR"

      step "Downloading TTS model (OpenMOSS-Team/MOSS-TTS-Local-Transformer)..."
      python3 -c "from huggingface_hub import snapshot_download; snapshot_download('OpenMOSS-Team/MOSS-TTS-Local-Transformer', local_dir='$TTS_DIR')"
      step "TTS model saved at: $TTS_DIR"

      step "Downloading TTS codec (OpenMOSS-Team/MOSS-Audio-Tokenizer)..."
      python3 -c "from huggingface_hub import snapshot_download; snapshot_download('OpenMOSS-Team/MOSS-Audio-Tokenizer', local_dir='$CODEC_DIR')"
      step "TTS codec saved at: $CODEC_DIR"

      step "All models ready."
      exit 0
    fi
    # -------------------------------------------------------------------------

    step 'Claude voice assistant — Ctrl+C to exit'
    step 'Tip: run with "init" first to pre-download the model'
    printf '\n' >&2

    while true; do
      printf 'Press Enter to start recording...\n' >&2
      read -r _ || break

      # Start sox; fall back to ffmpeg if it exits within 0.5s
      step 'Starting recorder (sox)...'
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

      printf 'Recording... press Enter to stop.\n' >&2
      read -r _ || { kill -INT "$RECORD_PID" 2>/dev/null; break; }
      kill -INT "$RECORD_PID" 2>/dev/null
      wait "$RECORD_PID" 2>/dev/null || true

      if [[ ! -s "$TMPFILE" ]]; then
        step 'No audio captured (file is empty)'
        continue
      fi

      step "Recorded $(wc -c < "$TMPFILE") bytes — transcribing with parakeet-mlx..."

      # parakeet-mlx writes a sidecar file next to the input; --format txt
      # gives us clean text without SRT timestamps
      TXT_OUT="''${TMPFILE%.wav}.txt"
      # Run from TMPFILE's directory so parakeet-mlx writes the sidecar there
      ( cd "$(dirname "$TMPFILE")" && parakeet-mlx --output-format txt "$TMPFILE" ) && PARAKEET_EXIT=0 || PARAKEET_EXIT=$?

      if [[ $PARAKEET_EXIT -ne 0 ]]; then
        step "parakeet-mlx exited with code $PARAKEET_EXIT"
      fi

      TRANSCRIPT=$(cat "$TXT_OUT" 2>/dev/null || true)
      rm -f "$TXT_OUT"
      TRANSCRIPT=$(printf '%s' "$TRANSCRIPT" | tr -d '\n' | sed 's/^[[:space:]]*//')

      if [[ -z "$TRANSCRIPT" ]]; then
        step 'No speech detected in audio'
        continue
      fi

      step 'Transcript done — calling Claude...'
      printf 'You: %s\n' "$TRANSCRIPT" >&2

      if $FIRST_TURN; then
        RESPONSE=$(claude --append-system-prompt "$VOICE_PROMPT" -p "$TRANSCRIPT" 2>&1) || true
        FIRST_TURN=false
      else
        RESPONSE=$(claude -c --append-system-prompt "$VOICE_PROMPT" -p "$TRANSCRIPT" 2>&1) || true
      fi

      if [[ -z "$RESPONSE" ]]; then
        step 'No response from Claude'
        continue
      fi

      step 'Speaking response...'
      printf 'Claude: %s\n\n' "$RESPONSE" >&2
      MLX_SPEECH_MODELS="''${MLX_SPEECH_MODELS:-$HOME/.local/share/mlx-speech/models}"
      if [[ -d "$MLX_SPEECH_MODELS/openmoss/moss_tts_local/mlx-int8" ]]; then
        step 'TTS: using mlx-speak'
        MLX_SPEECH_MODELS="$MLX_SPEECH_MODELS" mlx-speak "$RESPONSE" || {
          step "mlx-speak failed (exit $?), falling back to say"
          /usr/bin/say "$RESPONSE"
        }
      else
        step 'TTS: mlx-speech models not found, using say (run "init" to download)'
        /usr/bin/say "$RESPONSE"
      fi
    done
  '';

  meta = with lib; {
    description = "Voice assistant: mic → parakeet-mlx → Claude → say";
    platforms = [ "aarch64-darwin" ];
  };
}
