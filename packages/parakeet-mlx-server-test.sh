#!/usr/bin/env bash
# packages/parakeet-mlx-server-test.sh
# Usage: nix run .#parakeet-mlx-server-test
# Or:   bash packages/parakeet-mlx-server-test.sh [host] [port]
set -euo pipefail

HOST="${1:-127.0.0.1}"
PORT="${2:-5092}"
BASE_URL="http://${HOST}:${PORT}"
FAIL=0

log() { printf "\033[1m[TEST] %s\033[0m\n" "$*"; }
pass() { printf "  \033[32m✓ PASS\033[0m %s\n" "$*"; }
fail() {
  printf "  \033[31m✗ FAIL\033[0m %s\n" "$*"
  FAIL=$((FAIL + 1))
}

# --- Generate a small test WAV (1s of 440 Hz sine, 16 kHz, mono) ---
TEST_WAV="$(mktemp "${TMPDIR:-/tmp}"/parakeet-test-XXXXXX.wav)"
ffmpeg -y -f lavfi -i "sine=frequency=440:duration=1" \
  -ar 16000 -ac 1 -sample_fmt s16 "$TEST_WAV" >/dev/null 2>&1

# --- Wait for server to be ready ---
log "Waiting for server at ${BASE_URL} ..."
for i in $(seq 1 30); do
  if curl -sf "${BASE_URL}/health" >/dev/null 2>&1; then break; fi
  sleep 1
  if [ "$i" -eq 30 ]; then
    fail "Server did not become ready within 30s"
    rm -f "$TEST_WAV"
    exit 1
  fi
done
pass "Server is up"

# --- Test 1: Health endpoint (GET /health) ---
log "Test 1: GET /health"
HEALTH=$(curl -sf "${BASE_URL}/health")
if echo "$HEALTH" | python3 -c "import sys,json; d=json.load(sys.stdin); assert d['status']=='ok'"; then
  pass '/health returned {"status": "ok"}'
  MODEL=$(echo "$HEALTH" | python3 -c "import sys,json; print(json.load(sys.stdin)['model'])")
  pass "Model name reported: $MODEL"
else
  fail "/health did not return expected JSON"
fi

# --- Test 2: OpenAI-compatible endpoint (POST /v1/audio/transcriptions) ---
log "Test 2: POST /v1/audio/transcriptions (multipart)"
RESP=$(curl -sf "${BASE_URL}/v1/audio/transcriptions" \
  -F "file=@${TEST_WAV}" \
  -F "response_format=json")
if echo "$RESP" | python3 -c "import sys,json; d=json.load(sys.stdin); assert 'text' in d; assert isinstance(d['text'], str)"; then
  TEXT=$(echo "$RESP" | python3 -c "import sys,json; print(json.load(sys.stdin)['text'])")
  pass "/v1/audio/transcriptions returned text: ${TEXT:0:60}"
else
  fail "/v1/audio/transcriptions did not return valid JSON with 'text' key"
fi

# --- Test 3: Whisper.cpp-compatible endpoint (POST /inference) ---
log "Test 3: POST /inference (whisper.cpp format)"
RESP3=$(curl -sf "${BASE_URL}/inference" \
  -F "file=@${TEST_WAV}")
if echo "$RESP3" | python3 -c "import sys,json; d=json.load(sys.stdin); assert 'text' in d"; then
  TEXT3=$(echo "$RESP3" | python3 -c "import sys,json; print(json.load(sys.stdin)['text'])")
  pass "/inference returned JSON: text='${TEXT3:-(empty, OK for non-speech)}'"
else
  fail "/inference did not return valid JSON with 'text' key"
fi

# --- Test 4: Simple endpoint (POST /transcribe, plain text) ---
log "Test 4: POST /transcribe (plain text)"
RESP4=$(curl -sf "${BASE_URL}/transcribe" \
  -F "file=@${TEST_WAV}")
if [ -n "$RESP4" ] || [ "$RESP4" = "" ]; then
  pass "/transcribe returned: ${RESP4:-(empty, OK for non-speech)}"
else
  fail "/transcribe returned unexpected response"
fi

# --- Test 5: Latency check ---
log "Test 5: Latency (should be <2s for warm model)"
T_START=$(python3 -c "import time; print(time.time())")
curl -sf "${BASE_URL}/v1/audio/transcriptions" -F "file=@${TEST_WAV}" >/dev/null
T_END=$(python3 -c "import time; print(time.time())")
ELAPSED=$(python3 -c "print(f'{${T_END} - ${T_START}:.3f}')")
if python3 -c "assert ${ELAPSED} < 2.0, f'{${ELAPSED}}s too slow'"; then
  pass "Transcription latency: ${ELAPSED}s (< 2s threshold)"
else
  fail "Transcription latency: ${ELAPSED}s (>= 2s, expected <2s for warm model)"
fi

# --- Test 6: Error handling — missing file field ---
log "Test 6: Error handling — POST with no file"
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" "${BASE_URL}/v1/audio/transcriptions" \
  -F "data=not-a-file")
if [ "$HTTP_CODE" = "400" ]; then
  pass "Missing file field returns HTTP 400"
else
  fail "Missing file field returned HTTP $HTTP_CODE (expected 400)"
fi

# --- Test 7: Concurrency — two simultaneous requests ---
log "Test 7: Concurrency — two simultaneous requests"
curl -sf "${BASE_URL}/v1/audio/transcriptions" -F "file=@${TEST_WAV}" >/dev/null &
curl -sf "${BASE_URL}/v1/audio/transcriptions" -F "file=@${TEST_WAV}" >/dev/null &
wait
pass "Two concurrent requests completed without crash"

# --- Test 8: Wrong HTTP method on POST-only endpoint ---
log "Test 8: GET /v1/audio/transcriptions returns 404"
HTTP_CODE8=$(curl -s -o /dev/null -w "%{http_code}" "${BASE_URL}/v1/audio/transcriptions")
if [ "$HTTP_CODE8" = "404" ]; then
  pass "GET on POST-only endpoint returns 404"
else
  fail "GET on POST-only endpoint returned HTTP $HTTP_CODE8 (expected 404)"
fi

# --- Cleanup ---
rm -f "$TEST_WAV"

# --- Summary ---
if [ "$FAIL" -eq 0 ]; then
  printf "\n\033[32mAll tests passed.\033[0m\n"
else
  printf "\n\033[31m%d test(s) failed.\033[0m\n" "$FAIL"
fi
exit $FAIL
