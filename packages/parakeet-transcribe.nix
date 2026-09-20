{
  lib,
  writeShellApplication,
  sox,
  ffmpeg,
  parakeet-mlx,
}:

writeShellApplication {
  name = "parakeet-transcribe";

  runtimeInputs = [
    sox
    ffmpeg
    parakeet-mlx
  ];

  text = ''
    TMPFILE=$(mktemp /tmp/parakeet-XXXXXX.wav)
    trap 'rm -f "$TMPFILE"' EXIT

    printf 'Recording... press Enter to stop, Ctrl+C to cancel.\n' >&2

    # Start sox; if it exits within 0.5s the audio device failed to open
    sox -d -r 16000 -c 1 -b 16 "$TMPFILE" 2>/dev/null &
    RECORD_PID=$!
    sleep 0.5

    if ! kill -0 "$RECORD_PID" 2>/dev/null; then
      printf 'sox failed, falling back to ffmpeg (avfoundation)...\n' >&2
      ffmpeg -f avfoundation -i ":0" \
        -ar 16000 -ac 1 -acodec pcm_s16le \
        -y -loglevel error "$TMPFILE" &
      RECORD_PID=$!
    fi

    read -r _
    kill -INT "$RECORD_PID" 2>/dev/null
    wait "$RECORD_PID" 2>/dev/null || true

    if [[ ! -s "$TMPFILE" ]]; then
      printf 'No audio captured.\n' >&2
      exit 1
    fi

    printf 'Transcribing...\n' >&2
    parakeet-mlx "$TMPFILE"
  '';

  meta = with lib; {
    description = "Record mic audio and transcribe with parakeet-mlx";
    platforms = [ "aarch64-darwin" ];
  };
}
