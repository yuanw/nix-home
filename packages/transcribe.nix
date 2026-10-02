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
  writeShellApplication,
  yt-dlp,
  ffmpeg,
  whisper-cpp,
  gawk,
  coreutils,
  findutils,
  gnugrep,
  gnused,
  # The speech-to-text CLI called when a video has no captions.  Only put on
  # PATH on the two platforms that ship it; tests pass a stub.
  # Path to srt2txt.awk (injected so tests can pass their own copy).
  srt2txt,
  ...
}:
let
  isDarwin = stdenv.hostPlatform.isDarwin;

  preamble = ''
    set -euo pipefail
    step() { printf '[%s] %s\n' "''${0##*/}" "$*" >&2; }
    die() { step "$*"; exit 1; }
    newest_in() { # newest_in DIR PATTERN -> newest match, or empty
      find "$1" -maxdepth 1 -type f -name "$2" 2>/dev/null | sort | tail -1
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
  # What the wrapped script may look up by name.  cohere-transcribe is only
  # where it is shipped, and its absence on x86_64-* is a missing attribute
  # rather than a skipped backend, so it is added by platform.
  asrPlatforms = [
    "aarch64-darwin"
    "aarch64-linux"
  ];
in
{
  transcribe = writeShellApplication {
    name = "transcribe";
    runtimeInputs = [
      coreutils
      findutils
      gnugrep
      gnused
      gawk
      ffmpeg
      yt-dlp
      whisper-cpp
    ]
    ++ lib.optionals (builtins.elem stdenv.hostPlatform.system asrPlatforms) [ cohere-transcribe ];

    text = ''
      ${preamble}

      sub_langs="''${TRANSCRIBE_SUB_LANGS:-en.,en}"
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
        step "             WHISPER_MODEL (ggml model file for the bundled whisper-cpp),"
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

      asr() { # asr FILE -> transcript text on stdout
        file="$(realpath "$1")"
        if [[ -n "''${TRANSCRIBE_ASR_CMD:-}" ]]; then
          # TRANSCRIBE_ASR_CMD is word-split on spaces, the media file is
          # appended as its own argument so paths may contain spaces.
          # e.g. TRANSCRIBE_ASR_CMD='parakeet-mlx --output-format txt'
          read -ra cmd <<< "$TRANSCRIBE_ASR_CMD"
          "''${cmd[@]}" "$file"
        elif [[ -n "''${WHISPER_MODEL:-}" ]] && command -v whisper-cpp >/dev/null; then
          whisper-cpp -m "$WHISPER_MODEL" -f "$file" -nt
        elif [[ -n "''${COHERE_TRANSCRIBE_MODEL_DIR:-}" ]] \
          && command -v cohere-transcribe >/dev/null; then
          # The weights live in a directory of their own, named by hand: the
          # file is a positional argument, and --model-dir has to name the
          # directory holding config.json, model.safetensors and vocab.json.
          # $HOME and ~ do not expand inside TRANSCRIBE_ASR_CMD -- word
          # splitting is not tilde expansion -- which is why the model
          # directory is a variable of its own instead of part of that one.
          cohere-transcribe --model-dir "''${COHERE_TRANSCRIBE_MODEL_DIR}" "$file"
        else
          die "no speech-to-text backend: set \$TRANSCRIBE_ASR_CMD, \$WHISPER_MODEL or \$COHERE_TRANSCRIBE_MODEL_DIR, or pass --subs-only"
        fi
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
          asr "$audio" | sed '/^[[:space:]]*$/d' > "$tmp"
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
