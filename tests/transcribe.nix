/*
  Checks for packages/transcribe.nix.

  The scripts under test talk to yt-dlp, whisper-cpp and cohere-transcribe;
  all three arrive as stubs (a real cohere-transcribe would be fetched from
  GitHub), so what gets exercised is our own logic: caption vs audio
  routing, the SRT cleanup, cookie flag construction, and which
  speech-to-text backend gets asked with what.  ffmpeg is real, and so are
  the media fixtures: whatever reaches a backend has to decode, and empty
  stand-ins would now stop at the transcoding step instead.
*/
{
  pkgs,
  lib,
}:
let
  # stub NAME SCRIPT -> a derivation with bin/NAME on it
  stub =
    name: text:
    let
      script = pkgs.writeText "${name}-stub" ''
        #!${pkgs.bash}/bin/bash
        set -euo pipefail
        ${text}
      '';
    in
    pkgs.runCommand "${name}-stub" { } ''
      mkdir -p "$out/bin"
      cp ${script} "$out/bin/${name}"
      chmod +x "$out/bin/${name}"
    '';

  # Records its argv, then fabricates whatever the caller asked for.
  #   YTDLP_STUB_LOG    file to append argv to
  #   YTDLP_STUB_SRT    srt to emit for --write-subs   ("" -> emit nothing)
  #   YTDLP_STUB_AUDIO  media to emit for -x           ("" -> emit nothing)
  ytDlpStub = stub "yt-dlp" ''
    log="''${YTDLP_STUB_LOG:-}"
    argv="$*"   # the loop below shifts $@ away, so keep the argv copy
    emit_srt="''${YTDLP_STUB_SRT:-}"
    emit_audio="''${YTDLP_STUB_AUDIO:-}"
    o=""
    subs=0
    audio=0
    while [[ $# -gt 0 ]]; do
      case "$1" in
        -o) o="''${2:?-o needs an argument}" ;;
        --print) # --print "%(id)s": the caller wants the video id on stdout
          [[ -z "''${YTDLP_STUB_ID:-}" ]] || printf '%s\n' "$YTDLP_STUB_ID"
          exit 0 ;;
        --write-subs) subs=1 ;;
        -x) audio=1 ;;
        *) ;;
      esac
      shift
    done
    [[ -z "$log" ]] || printf '%s\n' "$argv" >> "$log"
    [[ -n "$o" ]] || exit 0
    d="$(dirname "$o")"
    mkdir -p "$d"
    if [[ "$subs" == 1 && -n "$emit_srt" ]]; then cp "$emit_srt" "$d/stub.en.srt"; fi
    if [[ "$audio" == 1 && -n "$emit_audio" ]]; then cp "$emit_audio" "$d/stub.m4a"; fi
  '';

  # Stands in for pkgs.cohere-transcribe, which the script may also look up by
  # name.  What matters here is that the model *directory* and the media file
  # reach it as two separate arguments.
  cohereStub = stub "cohere-transcribe" ''
    model=""
    file=""
    while [[ $# -gt 0 ]]; do
      case "$1" in
        --model-dir) model="''${2:?}" ;;
        *) file="''${1:?}" ;;
      esac
      shift
    done
    [[ -d "$model" ]] || { echo "stub cohere-transcribe: not a directory: $model" >&2; exit 4; }
    [[ -f "$file" ]] || { echo "stub cohere-transcribe: not a file: $file" >&2; exit 4; }
    printf 'cohere transcript of %s\n' "$(basename "$file")"
  '';

  whisperStub = stub "whisper-cpp" ''
    model=""
    file=""
    while [[ $# -gt 0 ]]; do
      case "$1" in
        -m) model="''${2:?}" ;;
        -f) file="''${2:?}" ;;
        *) ;;
      esac
      shift
    done
    if [[ -z "$model" || ! -f "$file" ]]; then echo "stub whisper-cpp: bad -m/-f" >&2; exit 3; fi
    printf 'whisper transcript of %s\n' "$(basename "$file")"
  '';

  asrStub = stub "asr-cmd" ''
    printf 'cmd transcript of %s\n' "$(basename "''${1:?usage: asr-cmd FILE}")"
    [[ -z "''${ASR_STUB_SEEN:-}" ]] || printf '%s\n' "$1" > "$ASR_STUB_SEEN"
  '';

  # Stands in for a backend that answers every call with the same text, which
  # is the only way to script what the model would have said.  CAT_FILE names
  # that answer; CAT_SEEN collects the file behind each call.
  catStub = stub "cat-file" ''
    answer="''${CAT_FILE:?}"
    printf '%s\n' "$(cat "$answer")"
    [[ -z "''${CAT_SEEN:-}" ]] || printf '%s\n' "$1" >> "$CAT_SEEN"
  '';

  # A stand-in for a model that is transcribing correctly: real English prose
  # from docs/, three times over.  Big enough to judge on its own -- prose
  # compresses to about a quarter of its size, a model eating itself to a
  # thirteenth -- and not identical on every line, so three windows worth of
  # it reads as three answers and not as one repetition loop.
  cleanAnswer = pkgs.runCommand "clean-answer.txt" { } ''
    for copy in 1 2 3; do
      cat ${./../docs}/*.md ${./../docs}/*.org
      printf '\n'
    done >> $out
  '';

  scripts = pkgs.callPackage ../packages/transcribe.nix {
    inherit (pkgs)
      lib
      stdenv
      writeShellApplication
      gawk
      coreutils
      findutils
      gnugrep
      gnused
      ffmpeg
      ;
    yt-dlp = ytDlpStub;
    whisper-cpp = whisperStub;
    cohere-transcribe = cohereStub;
    srt2txt = ../packages/srt2txt.awk;
  };

  transcribeBin = "${scripts.transcribe}/bin/transcribe";
  # Quoted attr, not `${scripts.yt-dlp-librewolf}`: inside an interpolation a
  # dash parses as subtraction.
  ytDlpLibrewolfBin = "${builtins.getAttr "yt-dlp-librewolf" scripts}/bin/yt-dlp-librewolf";
  asrBin = "${asrStub}/bin/asr-cmd";
  catFileBin = "${catStub}/bin/cat-file";
in
pkgs.runCommand "transcribe-tests"
  {
    nativeBuildInputs = [
      pkgs.bash
      pkgs.coreutils
      pkgs.gnugrep
      pkgs.gawk
      pkgs.ffmpeg
    ];
    meta.description = "Regression tests for transcribe and yt-dlp-librewolf";
  }
  ''
    set -euo pipefail
    fail() { printf 'FAIL: %s\n' "$*" >&2; exit 1; }
    rc=0
    transcribe="${transcribeBin}"
    wrapper="${ytDlpLibrewolfBin}"
    asrcmd="${asrBin}"
    catfile="${catFileBin}"
    clean="${cleanAnswer}"
    ytstubbin="${ytDlpStub}/bin/yt-dlp"

    mkdir -p tmp out
    # Caption fixture: two cues, one of them past the one-hour mark, plus the
    # frame numbers and timing lines that must not survive cleanup.
    cat > tmp/hello.srt <<EOF
    1
    00:00:01,280 --> 00:00:06,480
    Hi and welcome to bending emacs episode

    2
    01:02:03,000 --> 01:02:07,000
    one hour mark line
    EOF
    # Fixtures that would reach a speech-to-text backend must be decodable
    # media, not empty files: anything that is not WAV or MP3 goes through
    # ffmpeg first now, and ffmpeg on an empty file fails the same way a
    # missing audio track does -- before the stub backend ever runs.  A
    # 440 Hz sine is cheap real audio; the MP3 stays empty because MP3
    # skips that step and the stub backends only check that a file arrived.
    ffmpeg -v error -f lavfi -i sine=frequency=440:duration=1 -c:a aac tmp/hello.m4a
    ffmpeg -v error -f lavfi -i sine=frequency=440:duration=1 -c:a libopus tmp/video.webm
    : > tmp/audioonly.mp3
    cp tmp/hello.srt tmp/video.en.srt
    # A lecture-sized fixture: three minutes of tone, so a 30 s window means
    # three windows and one model call each.  MP3 on purpose: WAV and MP3 are
    # what the backends are known to decode, so this is the shape a lecture
    # arrives in, and a window has to be cut out of it before one is asked
    # to read it.
    ffmpeg -v error -f lavfi -t 180 -i sine=frequency=440 -c:a libmp3lame tmp/long.mp3

    # A lecture-sized answer that came back looping: one sentence, on repeat,
    # which is the shape the first lecture transcript had.  27 lines, which is
    # far too big to be true and far too compressible to be believed.
    {
      printf 'Is this amplification on?\n'
      for i in $(seq 25); do
        printf 'The emphasis was that the emphasis was that the emphasis was that\n'
      done
      printf 'And the upshot is true.\n'
    } > tmp/loop.txt

    export YTDLP_STUB_SRT="$PWD/tmp/hello.srt"
    export YTDLP_STUB_AUDIO="$PWD/tmp/hello.m4a"
    export YTDLP_STUB_LOG="$PWD/tmp/yt-dlp.log"
    export YTDLP_STUB_ID="dQw4w9WgXcQ"   # what --print "%(id)s" answers

    echo "[transcribe-tests] captions win over speech-to-text, timestamps on"
    : > "$YTDLP_STUB_LOG"
    COOKIE_BROWSER= "$transcribe" -o out "https://www.youtube.com/watch?v=dQw4w9WgXcQ" >/dev/null
    # Named after the video id, not after "watch" (every watch URL ends in that).
    [[ -f out/dQw4w9WgXcQ.txt ]] || fail "no transcript written: $(ls out)"
    grep -q '^\[00:01\] Hi and welcome to bending emacs episode$' out/dQw4w9WgXcQ.txt \
      || fail "timestamped cue wrong: $(cat out/dQw4w9WgXcQ.txt)"
    grep -q '^\[1:02:03\] one hour mark line$' out/dQw4w9WgXcQ.txt \
      || fail "hour-scale cue wrong: $(cat out/dQw4w9WgXcQ.txt)"
    if grep -qx '2' out/dQw4w9WgXcQ.txt; then fail "frame number leaked into transcript"; fi
    if grep -q -- '-->' out/dQw4w9WgXcQ.txt; then fail "timing line leaked into transcript"; fi
    grep -q -- '--write-subs' "$YTDLP_STUB_LOG" \
      || fail "no caption request reached yt-dlp: $(cat "$YTDLP_STUB_LOG")"
    if grep -q -- '--audio-format' "$YTDLP_STUB_LOG"; then
      fail "extracted audio even though captions were there: $(cat "$YTDLP_STUB_LOG")"
    fi

    echo "[transcribe-tests] -T drops the timestamps"
    COOKIE_BROWSER= "$transcribe" -T -o out "https://www.youtube.com/watch?v=dQw4w9WgXcQ" >/dev/null
    grep -qx 'Hi and welcome to bending emacs episode' out/dQw4w9WgXcQ.txt \
      || fail "plain mode wrong: $(cat out/dQw4w9WgXcQ.txt)"

    echo "[transcribe-tests] no video id -> fall back to the URL tail as the name"
    YTDLP_STUB_ID= COOKIE_BROWSER= "$transcribe" -o out "https://www.youtube.com/watch?v=noid" >/dev/null
    [[ -f out/watch.txt ]] || fail "fallback naming broken: $(ls out)"

    echo "[transcribe-tests] no captions -> audio -> whisper-cpp"
    : > "$YTDLP_STUB_LOG"
    YTDLP_STUB_ID=nosubs YTDLP_STUB_SRT= WHISPER_MODEL=/dev/null COOKIE_BROWSER= \
      "$transcribe" -o out "https://youtu.be/nosubs" >/dev/null
    grep -q -- '--audio-format' "$YTDLP_STUB_LOG" \
      || fail "no audio extraction was ever asked for: $(cat "$YTDLP_STUB_LOG")"
    if grep -q 'cookies-from-browser' "$YTDLP_STUB_LOG"; then
      fail "COOKIE_BROWSER= still asked for cookies: $(cat "$YTDLP_STUB_LOG")"
    fi
    grep -q '^whisper transcript of audio.wav$' out/nosubs.txt \
      || fail "whisper fallback did not run: $(cat out/nosubs.txt)"

    echo "[transcribe-tests] TRANSCRIBE_ASR_CMD outranks WHISPER_MODEL"
    : > tmp/asr-seen
    YTDLP_STUB_ID=cmd YTDLP_STUB_SRT= ASR_STUB_SEEN="$PWD/tmp/asr-seen" TRANSCRIBE_ASR_CMD="$asrcmd" \
    WHISPER_MODEL=/dev/null COOKIE_BROWSER= \
      "$transcribe" -o out "https://youtu.be/cmd" >/dev/null
    grep -q '^cmd transcript of audio.wav$' out/cmd.txt \
      || fail "explicit speech-to-text command ignored: $(cat out/cmd.txt)"
    [[ -f "$(cat tmp/asr-seen)" ]] || fail "command was handed a non-path: $(cat tmp/asr-seen)"

    echo "[transcribe-tests] the model directory reaches cohere-transcribe as --model-dir"
    mkdir -p tmp/models
    : > tmp/models/model.safetensors
    TRANSCRIBE_ASR_CMD="$asrcmd" COHERE_TRANSCRIBE_MODEL_DIR="$PWD/tmp/models" COOKIE_BROWSER= \
      "$transcribe" -o out tmp/audioonly.mp3 >/dev/null
    grep -q '^cmd transcript of audioonly.mp3$' out/audioonly.txt \
      || fail "TRANSCRIBE_ASR_CMD should still be the first choice: $(cat out/audioonly.txt)"
    # No TRANSCRIBE_ASR_CMD this time, so the model directory is what names
    # the backend.  The stub only prints a transcript when --model-dir names a
    # real directory and the audio file arrives as a file of its own.
    COHERE_TRANSCRIBE_MODEL_DIR="$PWD/tmp/models" COOKIE_BROWSER= \
      "$transcribe" -o out tmp/audioonly.mp3 >/dev/null
    grep -q '^cohere transcript of audioonly.mp3$' out/audioonly.txt \
      || fail "model directory ignored: $(cat out/audioonly.txt)"

    echo "[transcribe-tests] --force-asr ignores captions that would have worked"
    : > "$YTDLP_STUB_LOG"
    : > tmp/asr-seen
    YTDLP_STUB_ID=force ASR_STUB_SEEN="$PWD/tmp/asr-seen" TRANSCRIBE_ASR_CMD="$asrcmd" COOKIE_BROWSER= \
      "$transcribe" -A -o out "https://youtu.be/force" >/dev/null
    if grep -q -- '--write-subs' "$YTDLP_STUB_LOG"; then
      fail "--force-asr still looked for captions: $(cat "$YTDLP_STUB_LOG")"
    fi
    grep -q -- '--audio-format' "$YTDLP_STUB_LOG" \
      || fail "--force-asr never extracted audio: $(cat "$YTDLP_STUB_LOG")"
    grep -q '^cmd transcript of audio.wav$' out/force.txt \
      || fail "--force-asr skipped speech-to-text: $(cat out/force.txt)"

    echo "[transcribe-tests] --force-asr ignores a sidecar srt next to a local file"
    TRANSCRIBE_ASR_CMD="$asrcmd" COOKIE_BROWSER= "$transcribe" -A -o out tmp/video.webm >/dev/null
    grep -q '^cmd transcript of audio.wav$' out/video.txt \
      || fail "sidecar still won over --force-asr: $(cat out/video.txt)"

    echo "[transcribe-tests] -n and -A contradict each other"
    rc=0
    COOKIE_BROWSER= "$transcribe" -n -A -o out "https://youtu.be/both" 2>tmp/both.err || rc=$?
    [[ $rc -ne 0 ]] || fail "both contradictory flags accepted"
    grep -q 'pick one' tmp/both.err || fail "unhelpful message: $(cat tmp/both.err)"

    echo "[transcribe-tests] no backend at all -> a message naming the fix"
    rc=0
    WHISPER_MODEL= TRANSCRIBE_ASR_CMD= COHERE_TRANSCRIBE_MODEL_DIR= COOKIE_BROWSER= \
      "$transcribe" -o out tmp/audioonly.mp3 2>tmp/nobackend.err || rc=$?
    [[ $rc -ne 0 ]] || fail "succeeded with no speech-to-text backend"
    grep -q 'TRANSCRIBE_ASR_CMD' tmp/nobackend.err \
      || fail "message does not name the fix: $(cat tmp/nobackend.err)"

    echo "[transcribe-tests] --subs-only never reaches the audio path"
    rc=0
    YTDLP_STUB_SRT= COOKIE_BROWSER= "$transcribe" -n -o out "https://youtu.be/nosubsonly" 2>tmp/subsonly.err || rc=$?
    [[ $rc -ne 0 ]] || fail "--subs-only fell back to speech-to-text"
    grep -q 'no captions' tmp/subsonly.err \
      || fail "wrong --subs-only failure: $(cat tmp/subsonly.err)"

    echo "[transcribe-tests] URL with neither captions nor audio fails loudly"
    rc=0
    YTDLP_STUB_SRT= YTDLP_STUB_AUDIO= WHISPER_MODEL=/dev/null COOKIE_BROWSER= \
      "$transcribe" -o out "https://youtu.be/empty" 2>tmp/empty.err || rc=$?
    [[ $rc -ne 0 ]] || fail "succeeded with nothing to transcribe"
    grep -q 'nothing to transcribe' tmp/empty.err \
      || fail "wrong empty-media failure: $(cat tmp/empty.err)"

    echo "[transcribe-tests] local file prefers sidecar captions"
    COOKIE_BROWSER= "$transcribe" -o out tmp/video.webm >/dev/null
    grep -q '^\[00:01\] Hi and welcome' out/video.txt \
      || fail "sidecar transcript wrong: $(cat out/video.txt)"

    echo "[transcribe-tests] local file without sidecar goes to speech-to-text"
    WHISPER_MODEL=/dev/null COOKIE_BROWSER= "$transcribe" -o out tmp/audioonly.mp3 >/dev/null
    grep -q '^whisper transcript of audioonly.mp3$' out/audioonly.txt \
      || fail "local speech-to-text wrong: $(cat out/audioonly.txt)"

    echo "[transcribe-tests] a media file without an audio track fails loudly"
    ffmpeg -v error -f lavfi -i "color=c=black:s=64x64:d=1" -an tmp/silent.mp4
    rc=0
    WHISPER_MODEL=/dev/null TRANSCRIBE_ASR_CMD="$asrcmd" COOKIE_BROWSER= \
      "$transcribe" -o out tmp/silent.mp4 2>tmp/silent.err || rc=$?
    [[ $rc -ne 0 ]] || fail "a video with no audio track reached a backend"
    grep -q 'ffmpeg could not decode' tmp/silent.err \
      || fail "wrong no-audio failure: $(cat tmp/silent.err)"

    echo "[transcribe-tests] a lecture is transcribed in windows, one model call each"
    : > tmp/window-calls
    rc=0
    TRANSCRIBE_ASR_CHUNK=30 TRANSCRIBE_ASR_CMD="$catfile" CAT_FILE="$clean" \
      CAT_SEEN="$PWD/tmp/window-calls" "$transcribe" -o tmp/windowed tmp/long.mp3 \
      >tmp/windowed.log 2>&1 || rc=$?
    [[ $rc -eq 0 ]] || fail "clean speech was refused: $(tail -n 1 tmp/windowed.log)"
    # Seven, not six: the segment muxer cuts just after the window length, so
    # a 3-minute file at a 30 s window is six whole windows and a 26 ms sliver.
    [[ $(grep -c . tmp/window-calls) -eq 7 ]] \
      || fail "30 s windows in a 3-minute file should mean seven calls: $(grep -c . tmp/window-calls)"
    [[ -s tmp/windowed/long.txt ]] || fail "the windows were transcribed and nothing was written"

    echo "[transcribe-tests] ...and stops at the window that comes back looping"
    : > tmp/looping-calls
    rc=0
    TRANSCRIBE_ASR_CHUNK=30 TRANSCRIBE_ASR_CMD="$catfile" CAT_FILE="$PWD/tmp/loop.txt" \
      CAT_SEEN="$PWD/tmp/looping-calls" "$transcribe" -o tmp/looped tmp/long.mp3 \
      >tmp/looped.log 2>&1 || rc=$?
    if [[ $rc -eq 0 ]]; then fail "a model eating itself was filed as a transcript"; fi
    if ! grep -q 'repetition loop' tmp/looped.log; then
      fail "looping text was accepted without a word about it: $(tail -n 1 tmp/looped.log)"
    fi
    [[ $(grep -c . tmp/looping-calls) -eq 1 ]] \
      || fail "it should have stopped at the first window: $(grep -c . tmp/looping-calls)"

    echo "[transcribe-tests] TRANSCRIBE_REPEAT_RATIO=0 turns the looping check off"
    rc=0
    TRANSCRIBE_REPEAT_RATIO=0 TRANSCRIBE_ASR_CHUNK=30 TRANSCRIBE_ASR_CMD="$catfile" \
      CAT_FILE="$PWD/tmp/loop.txt" \
      "$transcribe" -o tmp/unchecked tmp/long.mp3 >tmp/unchecked.log 2>&1 || rc=$?
    [[ $rc -eq 0 ]] || fail "the looping check ran although TRANSCRIBE_REPEAT_RATIO=0"
    [[ -s tmp/unchecked/long.txt ]] || fail "with the check off the looping answer should still be filed"

    echo "[transcribe-tests] TRANSCRIBE_ASR_CHUNK=0 means one pass over the whole file"
    : > tmp/whole-call
    rc=0
    TRANSCRIBE_ASR_CHUNK=0 TRANSCRIBE_ASR_CMD="$catfile" CAT_FILE="$clean" \
      CAT_SEEN="$PWD/tmp/whole-call" "$transcribe" -o tmp/whole tmp/long.mp3 \
      >tmp/whole.log 2>&1 || rc=$?
    [[ $rc -eq 0 ]] || fail "one pass over a lecture was refused: $(cat tmp/whole.log)"
    [[ $(grep -c . tmp/whole-call) -eq 1 ]] \
      || fail "TRANSCRIBE_ASR_CHUNK=0 still cut the file up: $(grep -c . tmp/whole-call)"

    echo "[transcribe-tests] TRANSCRIBE_ASR_CMD is asked per window, file and all"
    rc=0
    TRANSCRIBE_ASR_CHUNK=30 TRANSCRIBE_ASR_CMD="$asrcmd" \
      "$transcribe" -o tmp/bycmd tmp/long.mp3 >tmp/bycmd.log 2>&1 || rc=$?
    [[ $rc -eq 0 ]] || fail "a custom speech-to-text command was refused: $(tail -n 1 tmp/bycmd.log)"
    [[ $(grep -c '^cmd transcript of ' tmp/bycmd/long.txt) -eq 7 ]] \
      || fail "not one answer per window: $(grep -c '^cmd transcript of ' tmp/bycmd/long.txt)"
    grep -q '^cmd transcript of window-006.wav$' tmp/bycmd/long.txt \
      || fail "the last window went unheard: $(tail -n 2 tmp/bycmd/long.txt)"

    echo "[transcribe-tests] with both backends on PATH, whisper-cpp is the one that gets asked"
    mkdir -p tmp/models2
    : > tmp/models2/model.safetensors
    rc=0
    WHISPER_MODEL=/dev/null TRANSCRIBE_ASR_LANG=de \
      COHERE_TRANSCRIBE_MODEL_DIR="$PWD/tmp/models2" TRANSCRIBE_ASR_CMD= \
      "$transcribe" -o tmp/either tmp/long.mp3 >tmp/either.log 2>&1 || rc=$?
    [[ $rc -eq 0 ]] || fail "nothing transcribed although two backends were on PATH: $(tail -n 1 tmp/either.log)"
    grep -q '^whisper transcript of long.mp3$' tmp/either/long.txt \
      || fail "wrong backend, or the file was not passed as one whole argument: $(cat tmp/either/long.txt)"

    echo "[transcribe-tests] a missing profile does not stop public videos"
    YTDLP_STUB_ID=public LIBREWOLF_PROFILE_ROOT="$PWD/tmp/nope" "$transcribe" -o out "https://youtu.be/public" >/dev/null
    grep -q '^\[00:01\] Hi and welcome' out/public.txt \
      || fail "anonymous download broke: $(cat out/public.txt)"

    echo "[transcribe-tests] yt-dlp-librewolf hands yt-dlp the profile root"
    mkdir -p tmp/profile
    : > tmp/profile/profiles.ini
    : > "$YTDLP_STUB_LOG"
    LIBREWOLF_PROFILE_ROOT="$PWD/tmp/profile" "$wrapper" --version
    grep -qx -- "--cookies-from-browser firefox:$PWD/tmp/profile --version" "$YTDLP_STUB_LOG" \
      || fail "cookie flag not assembled: $(cat "$YTDLP_STUB_LOG")"

    echo "[transcribe-tests] COOKIE_BROWSER= asks for no cookies"
    : > "$YTDLP_STUB_LOG"
    COOKIE_BROWSER= LIBREWOLF_PROFILE_ROOT="$PWD/tmp/profile" "$wrapper" --version >/dev/null
    if grep -q 'cookies-from-browser' "$YTDLP_STUB_LOG"; then
      fail "sent cookies although COOKIE_BROWSER is empty: $(cat "$YTDLP_STUB_LOG")"
    fi

    echo "[transcribe-tests] a missing profile stops the cookie wrapper"
    rc=0
    LIBREWOLF_PROFILE_ROOT="$PWD/tmp/nope" "$wrapper" --version 2>tmp/missing.err || rc=$?
    [[ $rc -ne 0 ]] || fail "wrapper succeeded with a missing profile"
    grep -q 'LibreWolf profile' tmp/missing.err \
      || fail "unhelpful message about the profile: $(cat tmp/missing.err)"

    echo "[transcribe-tests] a relative profile root is rejected (~ does not expand)"
    rc=0
    ( cd tmp && LIBREWOLF_PROFILE_ROOT=".librewolf" "$wrapper" --version ) 2>tmp/relative.err || rc=$?
    [[ $rc -ne 0 ]] || fail "wrapper accepted a relative profile root"
    grep -q 'absolute' tmp/relative.err \
      || fail "unhelpful message about the relative path: $(cat tmp/relative.err)"

    echo "[transcribe-tests] all checks passed"
    echo ok >"$out"
  ''
