{
  lib,
  writeShellApplication,
  sox,
  ffmpeg,
  parakeet-mlx,
}:

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
  ];

  text = ''
    TMPFILE=$(mktemp /tmp/claude-voice-XXXXXX.wav)
    FIRST_TURN=true
    trap 'rm -f "$TMPFILE"; printf "\nGoodbye.\n" >&2' EXIT

    VOICE_PROMPT=${lib.escapeShellArg voicePrompt}

    printf 'Claude voice assistant — Ctrl+C to exit.\n\n' >&2

    while true; do
      printf 'Press Enter to start recording...\n' >&2
      read -r _ || break

      # Start sox; fall back to ffmpeg if it exits within 0.5s
      sox -d -r 16000 -c 1 -b 16 "$TMPFILE" 2>/dev/null &
      RECORD_PID=$!
      sleep 0.5

      if ! kill -0 "$RECORD_PID" 2>/dev/null; then
        printf '(sox unavailable, using ffmpeg)\n' >&2
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
        printf 'No audio captured.\n' >&2
        continue
      fi

      printf 'Transcribing...\n' >&2
      TRANSCRIPT=$(parakeet-mlx "$TMPFILE" 2>/dev/null || true)
      TRANSCRIPT=$(printf '%s' "$TRANSCRIPT" | tr -d '\n' | sed 's/^[[:space:]]*//')

      if [[ -z "$TRANSCRIPT" ]]; then
        printf 'No speech detected.\n' >&2
        continue
      fi

      printf 'You: %s\n' "$TRANSCRIPT" >&2

      if $FIRST_TURN; then
        RESPONSE=$(claude --append-system-prompt "$VOICE_PROMPT" -p "$TRANSCRIPT" 2>&1) || true
        FIRST_TURN=false
      else
        RESPONSE=$(claude -c --append-system-prompt "$VOICE_PROMPT" -p "$TRANSCRIPT" 2>&1) || true
      fi

      if [[ -z "$RESPONSE" ]]; then
        printf 'No response from Claude.\n' >&2
        continue
      fi

      printf 'Claude: %s\n\n' "$RESPONSE" >&2
      /usr/bin/say "$RESPONSE"
    done
  '';

  meta = with lib; {
    description = "Voice assistant: mic → parakeet-mlx → Claude → say";
    platforms = [ "aarch64-darwin" ];
  };
}
