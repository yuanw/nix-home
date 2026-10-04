/*
  Media transcription helpers.

  transcribe         -- URL or media file -> plain text transcript
  yt-dlp-librewolf  -- yt-dlp that reads cookies out of the LibreWolf profile

  Both reach YouTube through yt-dlp, so they inherit its bot-check and
  age-restriction failures unless a cookie source is available: that is what
  yt-dlp-librewolf is for.  See cookieSetup below.
*/
{
  lib,
  stdenv,
  # Only for stdenv.hostPlatform.isDarwin below, keep `lib' and `stdenv' out
  # of the script's own PATH.
  writeShellApplication,
  yt-dlp,
  ffmpeg,
  whisper-cpp,
  gawk,
  gzip,
  coreutils,
  findutils,
  gnugrep,
  gnused,
  # The speech-to-text CLI used when a video has no captions.  Optional on
  # purpose: `asr' looks the name up on the caller's PATH, and hosts that ship
  # the CLI put it there (mist via environment.systemPath, and it is in the
  # script's own PATH too when this is called with an override, as the tests
  # do).  Baking it in would also drag a CUDA 13 package set into every
  # aarch64-linux build, where the CLI cannot run at all.
  cohere-transcribe ? null,
  # Path to srt2txt.awk (injected so tests can pass their own copy).
  srt2txt,
  ...
}:
let
  isDarwin = stdenv.hostPlatform.isDarwin;

  # `asr' cuts a long recording into windows of TRANSCRIBE_ASR_CHUNK seconds,
  # and hands one window to the model at a time.  Two things that buys:
  #
  #  - a model that stops understanding the audio keeps saying the last span
  #    it could still say, so the sooner a window starts the sooner that is
  #    noticed (see repetitive below), and the less of the lecture is spent
  #    before the run says so out loud;
  #  - the work per call is bounded, which is the only way a 16 GB host has of
  #    transcription with a model that has to page weights in -- the first
  #    lecture run spent ~27.5 minutes on one 58-minute call for exactly that
  #    reason.
  #
  # TRANSCRIBE_ASR_CHUNK=0 puts the single call back; TRANSCRIBE_REPEAT_RATIO=0
  # turns the repetition check off.  TRANSCRIBE_ASR_MAX_TOKENS is the cap on
  # what one call may spend: CohereTranscribe's own default is 448, which is
  # about a third of a windowful of lecture, so a full window comes back cut
  # off mid-sentence -- hence a knob, with a default big enough to finish.
  asrChunk = "\"\${TRANSCRIBE_ASR_CHUNK:-600}\"";
  repeatRatio = "\"\${TRANSCRIBE_REPEAT_RATIO:-4}\"";
  maxTokens = "\"\${TRANSCRIBE_ASR_MAX_TOKENS:-2048}\"";

  preamble = ''
    set -euo pipefail
    step() { printf '[%s] %s\n' "''${0##*/}" "$*" >&2; }
    die() { step "$*"; exit 1; }
    newest_in() { # newest_in DIR PATTERN -> newest match, or empty
      find "$1" -maxdepth 1 -type f -name "$2" 2>/dev/null | sort | tail -1
    }
    duration() { # duration FILE -> seconds, empty when ffprobe cannot say
      ffprobe -v error -show_entries format=duration -of csv=p=0 "$1" 2>/dev/null | tail -1
    }
    longer_than() { # longer_than FILE SECONDS
      local dur
      dur="$(duration "$1")"
      [[ -n "$dur" ]] && awk -v d="$dur" -v c="$2" 'BEGIN { exit !(d + 0 > c + 0) }'
    }
  '';

  # yt-dlp has no "librewolf" browser type (yt-dlp/yt-dlp#14050), so the
  # firefox cookie extractor has to be pointed at LibreWolf's profile root by
  # hand.  Two gotchas from that thread: yt-dlp does not expand `~`, and what
  # it wants is the profile *root* (the directory holding profiles.ini), not
  # one profile inside it.
  #
  # Cookie policy: COOKIE_BROWSER=librewolf (default) uses the platform profile
  # root, COOKIE_BROWSER=/some/path uses that, COOKIE_BROWSER= runs without
  # cookies.  A missing profile only warns here -- public videos still
  # download -- whereas the yt-dlp-librewolf wrapper treats it as fatal, since
  # cookies are the whole point of that one.
  defaultProfile =
    if isDarwin then "\$HOME/Library/Application Support/LibreWolf" else "\$HOME/.librewolf";

  cookieSetup = ''
    # No colon: an explicitly empty COOKIE_BROWSER means "no cookies", which
    # ''${COOKIE_BROWSER:-librewolf} would silently turn it back into librewolf.
    cookie_browser="''${COOKIE_BROWSER-librewolf}"
    cookie_flag=""
    if [[ -n "$cookie_browser" ]]; then
      profile="''${LIBREWOLF_PROFILE_ROOT:-${defaultProfile}}"
      [[ "$profile" == /* ]] \
        || die "LIBREWOLF_PROFILE_ROOT must be an absolute path, yt-dlp does not expand ~: $profile"
      if [[ -d "$profile" ]]; then
        cookie_flag="firefox:$profile"
      else
        step "warn: no LibreWolf profile at $profile, continuing without cookies (set \$LIBREWOLF_PROFILE_ROOT)"
      fi
    fi
  '';

  # What the wrapped script may look up by name.  cohere-transcribe is there
  # only when the caller passes one (the tests pass a stub; packages/default.nix
  # passes null, so a host that wants the CLI ships it through PATH -- see the
  # asr() branch below and hosts/mist.nix).
in
{
  transcribe = writeShellApplication {
    name = "transcribe";

    # ffprobe comes with ffmpeg, gzip and wc with coreutils.
    runtimeInputs = [
      coreutils
      findutils
      gnugrep
      gnused
      gawk
      gzip
      ffmpeg
      yt-dlp
      whisper-cpp
    ]
    ++ lib.optionals (cohere-transcribe != null) [ cohere-transcribe ];

    text = ''
      ${preamble}

      sub_langs="''${TRANSCRIBE_SUB_LANGS:-en.,en}"
      chunk_s=${asrChunk}
      cr_max=${repeatRatio}
      max_tokens=${maxTokens}
      subs_only=0
      force_asr=0
      timestamps=1
      outdir=""
      inputs=()

      usage() {
        step "usage: transcribe [-o DIR] [-n] [-A] [-T] URL_OR_FILE..."
        step "  -n, --subs-only       never fall back to speech-to-text"
        step "  -A, --force-asr       ignore captions, transcribe the audio (benchmarks)"
        step "  -T, --no-timestamps   drop the [mm:ss] prefixes"
        step "  -o, --output-dir DIR  where the .txt files land (default: \$PWD)"
        step "environment: TRANSCRIBE_SUB_LANGS, COOKIE_BROWSER, LIBREWOLF_PROFILE_ROOT,"
        step "             TRANSCRIBE_ASR_CMD (run as: \$TRANSCRIBE_ASR_CMD FILE),"
        step "             TRANSCRIBE_ASR_CHUNK (seconds of audio per model call, 0 = one call),"
        step "             TRANSCRIBE_REPEAT_RATIO (gzip size ratio above which the answer"
        step "             counts as a repetition loop, 0 = no check),"
        step "             TRANSCRIBE_ASR_MAX_TOKENS (2048, a whole window's worth; 0 = no cap),"
        step "             WHISPER_MODEL (ggml model file; whisper.cpp has to be on"
        step "             PATH too, under the name whisper-cli or whisper-cpp),"
        step "             COHERE_TRANSCRIBE_MODEL_DIR (directory holding the Cohere weights)"
        exit 2
      }

      while [[ $# -gt 0 ]]; do
        case "$1" in
          -n|--subs-only) subs_only=1 ;;
          -A|--force-asr) force_asr=1 ;;
          -T|--no-timestamps) timestamps=0 ;;
          -o|--output-dir) shift; [[ $# -gt 0 ]] || usage; outdir="$1" ;;
          -h|--help) usage ;;
          -*) die "unknown flag: $1" ;;
          *) inputs+=("$1") ;;
        esac
        shift
      done
      if [[ "$subs_only" -eq 1 && "$force_asr" -eq 1 ]]; then
        die "--subs-only refuses speech-to-text, --force-asr is speech-to-text: pick one"
      fi
      [[ ''${#inputs[@]} -gt 0 ]] || usage
      [[ -n "$outdir" ]] || outdir="$PWD"
      mkdir -p "$outdir"

      ${cookieSetup}

      run_yt_dlp() {
        if [[ -n "$cookie_flag" ]]; then
          yt-dlp --cookies-from-browser "$cookie_flag" "$@"
        else
          yt-dlp "$@"
        fi
      }

      # True when the answer that came back is the model eating itself.  The
      # gzip size ratio is the tell: text that repeats whole spans compresses
      # to a fraction of its size and text that does not.  Measured on this
      # machine -- clean English prose sits between 2.0 and 2.8 here, and the
      # 74 KB that came back looping from a lecture sat at 9.4 (23.7 counted
      # over the whole file).  Nothing that reads like someone talking comes
      # near 4, which is why nothing that reads like someone talking is ever
      # refused by this.
      repetitive() { # repetitive FILE SIZE RATIO -> true when FILE is looping
        [[ "$2" -gt 512 && "$3" != 0 ]] || return 1
        local gzipped
        gzipped="$(gzip -9 -c < "$1" | wc -c)"
        awk -v s="$2" -v g="$gzipped" -v r="$3" 'BEGIN { exit !((g > 0) && (s / g > r + 0)) }'
      }

      # Which backend does the transcription, and where the audio file goes in
      # its argv.  TRANSCRIBE_ASR_CMD is word-split on spaces and the media
      # file is appended as its own argument, so paths may contain spaces;
      # e.g. TRANSCRIBE_ASR_CMD='parakeet-mlx --output-format txt'.
      # Which backend does the transcription, and where the audio file goes in
      # its argv.  TRANSCRIBE_ASR_CMD is word-split on spaces and the media
      # file is appended as its own argument, so paths may contain spaces;
      # e.g. TRANSCRIBE_ASR_CMD='parakeet-mlx --output-format txt'.
      asr_run() { # asr_run FILE -> transcript text on stdout
        local -a cmd=()
        local wb=""
        if [[ -n "''${TRANSCRIBE_ASR_CMD:-}" ]]; then
          read -ra cmd <<< "$TRANSCRIBE_ASR_CMD"
          "''${cmd[@]}" "$1"
        elif [[ -f "''${WHISPER_MODEL:-}" ]]; then
          # brew's formula, and whisper.cpp upstream since 1.7.x, call the CLI
          # whisper-cli; nixpkgs still calls the package whisper-cpp.  Same
          # flags, different name, so look the name up rather than guess it.
          if command -v whisper-cli >/dev/null; then
            wb=whisper-cli
          elif command -v whisper-cpp >/dev/null; then
            wb=whisper-cpp
          else
            die "\$WHISPER_MODEL names a model but nothing on PATH would read it: install whisper.cpp (brew install whisper.cpp, or nixpkgs' whisper-cpp) and put it on PATH"
          fi
          # -nt drops the timestamps: without it every line comes back with a
          # [00:00:00] prefix, which is noise in a plain-text transcript.
          cmd=("$wb" -m "$WHISPER_MODEL" -f "$1" -nt)
          "''${cmd[@]}"
        elif [[ -n "''${COHERE_TRANSCRIBE_MODEL_DIR:-}" ]] \
          && command -v cohere-transcribe >/dev/null; then
          # The weights live in a directory of their own, named by hand: the
          # file is a positional argument, and --model-dir has to name the
          # directory holding config.json, model.safetensors and vocab.json.
          cmd=(cohere-transcribe --model-dir "''${COHERE_TRANSCRIBE_MODEL_DIR}" \
            --language en)
          if [[ "$max_tokens" != 0 ]]; then
            cmd+=(--max-tokens "$max_tokens")
          fi
          "''${cmd[@]}" "$1"
        else
          die "no speech-to-text backend: set \$TRANSCRIBE_ASR_CMD, \$WHISPER_MODEL or \$COHERE_TRANSCRIBE_MODEL_DIR, or pass --subs-only"
        fi
      }

      # Transcribe FILE into OUT, one model call per window of audio, and stop
      # at the first window that comes back looping instead of transcribing
      # the rest of the file on the strength of it.
      asr() { # asr FILE DIR OUT
        file="$(realpath "$1")"
        # What every backend here is *known* to decode is WAV and MP3; a
        # container or codec outside that gets PCM'd into DIR/audio.wav
        # first.  Not theory: the first real run handed the script an
        # Opus-in-WebM file and cohere-transcribe died with "Failed to
        # create audio decoder: unsupported codec", and whether the m4a
        # yt-dlp extracts decodes at all was never verified.  Mono
        # 16 kHz WAV is what both backends eat, so this is a detour that
        # cannot fail, not a format guess.
        case "''${file,,}" in
          *.wav|*.mp3) ;;
          *)
            step "decoding audio"
            ffmpeg -nostdin -loglevel error -i "$file" -vn -acodec pcm_s16le -ar 16000 -ac 1 "$2/audio.wav" \
              || die "ffmpeg could not decode $1 (no audio track?)"
            file="$2/audio.wav"
            ;;
        esac

        # A lecture is not a clip: one unbounded call holds all of it at once,
        # and a decoder that starts echoing a span back keeps doing it until
        # its token budget runs out.  Cutting the audio into windows gives the
        # model a way back out, gives `repetitive' above somewhere to notice
        # that it never found one, and stops the run at the first window that
        # comes back looping.  A file no longer than one window skips all of
        # this and is judged as one answer, which is what caught the lecture.
        if [[ "$chunk_s" != 0 ]] && longer_than "$file" "$chunk_s"; then
          step "transcribing $file in $chunk_s s windows"
          rm -f "$2"/window-*.wav "$2/window.txt"
          ffmpeg -nostdin -loglevel error -i "$file" -f segment -segment_time "$chunk_s" \
            -c copy "$2/window-%03d.wav" \
            || die "ffmpeg could not cut $file into $chunk_s s windows"
        fi

        if [[ -f "$2/window-000.wav" ]]; then
          windows="$(find "$2" -type f -name 'window-*.wav' | sort)"
        else
          windows="$file"
        fi

        : > "$3"
        while read -r window; do
          [[ -n "$window" ]] || continue
          step "speech-to-text: $(basename "$window")"
          if ! asr_run "$window" > "$2/window.txt"; then
            die "speech-to-text died on $window (does the model still run?)"
          fi
          if repetitive "$2/window.txt" "$(wc -c < "$2/window.txt")" "$cr_max"; then
            die "$(basename "$window") came back as a repetition loop: that is the model eating itself, not a transcript"
          fi
          sed '/^[[:space:]]*$/d' "$2/window.txt" >> "$3"
        done < <(printf '%s\n' "$windows")
      }

      for in in "''${inputs[@]}"; do
        work="$(mktemp -d)"
        mkdir -p "$work"
        srt=""

        if [[ "$in" == *://* ]]; then
          # A YouTube watch URL has the basename "watch", which would name every
          # transcript the same, so ask yt-dlp for the video id and only fall
          # back to the tail of the URL if it will not say.
          stem="$(run_yt_dlp --print "%(id)s" --skip-download "$in" 2>/dev/null || true)"
          if [[ -z "$stem" ]]; then
            stem="$(basename "''${in##*/}")"
            stem="''${stem%[?]*}"   # drop ?query: it has no business in a filename
            stem="''${stem%.*}"
          fi
          out="$outdir/$stem.txt"
          if [[ "$force_asr" -eq 0 ]]; then
            step "looking for captions: $in"
            run_yt_dlp --write-subs --write-auto-subs --sub-langs "$sub_langs" \
              --sub-format srt --skip-download --no-part \
              -o "$work/%(id)s.%(ext)s" "$in" || true
            srt="$(newest_in "$work" '*.srt')"
          fi
          if [[ -z "$srt" ]]; then
            if [[ "$subs_only" -eq 1 ]]; then
              die "no captions in $sub_langs for $in"
            fi
            [[ "$force_asr" -eq 0 ]] || step "captions not asked for (--force-asr)"
            step "extracting audio"
            run_yt_dlp -f "bestaudio/best" -x --audio-format m4a --no-part \
              -o "$work/%(id)s.%(ext)s" "$in"
          fi
        else
          [[ -f "$in" ]] || die "no such file: $in"
          stem="$(basename "''${in##*/}")"; stem="''${stem%.*}"
          out="$outdir/$stem.txt"
          if [[ "$subs_only" -eq 1 ]]; then
            die "--subs-only needs a URL, got a file: $in"
          fi
          # Sidecar captions beside the media beat a local speech-to-text run.
          if [[ "$force_asr" -eq 0 ]]; then
            srt="$(newest_in "$(dirname "$in")" "$stem*.srt")"
            [[ -n "$srt" ]] || step "no sidecar captions for $in, transcribing audio"
          fi
        fi

        tmp="$(mktemp "$work/out.XXXXXX")"
        if [[ -n "$srt" ]]; then
          step "captions: $srt"
          gawk -f ${srt2txt} -v timestamps="$timestamps" "$srt" > "$tmp"
        else
          audio="$(newest_in "$work" '*.m4a')"
          [[ -n "$audio" ]] || audio="$in"
          [[ -f "$audio" ]] || die "nothing to transcribe for $in"
          step "speech-to-text: $audio"
          asr "$audio" "$work" "$tmp"
        fi

        [[ -s "$tmp" ]] || die "empty transcript for $in"
        mv "$tmp" "$out"
        step "wrote $out"
        cat "$out"
      done
    '';

    meta.description = "Text transcript from a video URL or media file (captions first, speech-to-text fallback)";
  };

  yt-dlp-librewolf = writeShellApplication {
    name = "yt-dlp-librewolf";

    runtimeInputs = [ yt-dlp ];

    text = ''
      ${preamble}
      ${cookieSetup}
      # Unlike transcribe, which shrugs cookies off, this wrapper exists to be
      # authenticated, so a missing profile is fatal rather than a warning.
      if [[ -n "$cookie_browser" && -z "$cookie_flag" ]]; then
        die "no usable LibreWolf profile: $profile (fix \$LIBREWOLF_PROFILE_ROOT, or set COOKIE_BROWSER= for an anonymous run)"
      fi

      # Caller flags come last so they win over ours.
      if [[ -n "$cookie_flag" ]]; then
        exec yt-dlp --cookies-from-browser "$cookie_flag" "$@"
      else
        exec yt-dlp "$@"
      fi
    '';

    meta.description = "yt-dlp with YouTube cookies read from the LibreWolf profile (yt-dlp/yt-dlp#14050)";
  };
}
