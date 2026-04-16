# Plan: Parakeet-MLX Integration with whisper.el and Other Applications

## Current State Summary

### Machine
- **Apple M2 Max, 64 GB RAM** — ideal for MLX-accelerated inference

### What Already Works
| Component | Location | Status |
|-----------|----------|--------|
| `parakeet-mlx` Python package | `packages/parakeet-mlx.nix` | ✅ Packaged, works |
| `parakeet-transcribe` CLI wrapper | `packages/parakeet-transcribe.nix` | ✅ Record + transcribe |
| `speak2text` shell script | `modules/speak2text.nix` | ✅ Record → transcribe → clipboard |
| `speak2text-ptt-listener` | `modules/speak2text-ptt-listener.py` | ✅ Push-to-talk via macOS HID keycode |
| `claude-voice` | `packages/claude-voice.nix` | ✅ Mic → parakeet-mlx → Claude → `say` |
| `cohere-transcribe` | `packages/cohere-transcribe/` | ✅ Rust+MLX binary |
| `whisper.el` | `packages/emacs/whisper-el.nix` | ✅ Packaged |
| whisper.el config | `modules/editor/emacs/emacs-init-defaults.nix:288` | ⚠️ Hardcoded to **whisper.cpp** (`base.en` model) |
| speak2text → whisper.el bridge | `modules/speak2text.nix:241-252` | ⚠️ Overrides `whisper-command` to call CLI binary |
| `mlx-speak` / `mlx-speak-server` | `packages/mlx-speak*.nix` | ✅ TTS (text→speech), not STT |

### The Core Problem

**Every invocation of `parakeet-mlx` CLI loads the model from disk → Metal shader compilation → inference → exit.** On an M2 Max this is ~2–5 seconds per call. When whisper.el calls it for each short dictation segment, this cold-start latency makes interactive use feel sluggish compared to:

- Sacha Chua's **speaches** (persistent HTTP server, ~0.6–1.5s for a 10s clip on GPU)
- **whisper.cpp server mode** (`whisper-server`) which keeps the model in memory

### What Sacha Chua's Posts Teach Us

1. **First post (benchmarks):** She overrode `whisper--transcribe-via-local-server` and `whisper--ensure-server` to redirect whisper.el to any HTTP server exposing an OpenAI-compatible `/v1/audio/transcriptions` endpoint. She also overrode `whisper--check-model-consistency` to always return `t`. This pattern works with **any** transcription backend that can serve HTTP.

2. **Second post (workflow):** She uses `whisper.el` with `<f9>` toggle recording and a rich post-processing pipeline (`whisper-insert-text-at-point` function chain) to insert at point, save to Org clocked task, replay audio, etc. On her fork she added `whisper-insert-text-at-point` as a customizable function chain.

---

## Plan

### Phase 1: Parakeet-MLX HTTP Server (New Package)

**Goal:** Eliminate cold-start latency by keeping `parakeet-mlx` model loaded in a persistent daemon, exposed via a simple HTTP API.

#### 1.1 Create `packages/parakeet-mlx-server.nix`

A Python script that:
- Loads the model once at startup (`from_pretrained("mlx-community/parakeet-tdt-0.6b-v3")`)
- Warms up Metal JIT (transcribe a tiny silent clip)
- Runs an HTTP server on `127.0.0.1:${PARAKEET_MLX_PORT:-5092}`
- Exposes two endpoints:
  - `POST /v1/audio/transcriptions` — **OpenAI Whisper API-compatible** (returns `{"text": "..."}`), enabling whisper.el integration without fork patches
  - `POST /transcribe` — simpler endpoint accepting a WAV file, returns plain text
- Accepts model override via `?model=` query param (for future multi-model support)

```python
# Sketch of parakeet-mlx-server
from http.server import HTTPServer, BaseHTTPRequestHandler
import tempfile, os, json, time
from parakeet_mlx import from_pretrained
from parakeet_mlx.audio import load_audio

PORT = int(os.environ.get("PARAKEET_MLX_PORT", "5092"))
MODEL_NAME = os.environ.get("PARAKEET_MODEL", "mlx-community/parakeet-tdt-0.6b-v3")
model = from_pretrained(MODEL_NAME)

# Warm up Metal
_ = model.transcribe(load_audio("/dev/null", 16000))  # or a tiny WAV

class Handler(BaseHTTPRequestHandler):
    def do_POST(self):
        # Parse multipart form data
        # Save audio to temp file
        # result = model.transcribe(audio)
        # Return {"text": result.text}
        ...
```

Key design decisions:
- Use `http.server` from stdlib (no FastAPI/Flask dependency needed, keeps the Nix closure small)
- Or use `uvicorn` + `FastAPI` if we want async / better multipart handling (adds two deps)
- **Recommendation:** Start with stdlib `http.server` to minimize dependencies. For multipart parsing we need `cgi` module or `multipart` library. Actually, `email.parser` + `cgi` can handle it, but it's ugly. Let's use **Starlette** (already pulled in by `mlx-speech`) or just handle it manually.

Actually, since we already have `mlx-speech` in the closure and it likely pulls in starlette/httpx, let's check if we can reuse. But for simplicity and minimal closure, let's go with stdlib + `cgi` for multipart parsing.

#### 1.2 Add launchd Agent for the Server

In `modules/speak2text.nix`, add a launchd agent (Darwin-only) for the parakeet-mlx server:

```nix
launchd.user.agents.parakeet-mlx-server = lib.mkIf (cfg.enable && cfg.flavor == "parakeet-mlx") {
  serviceConfig = {
    ProgramArguments = [ "${parakeetMlxServer}/bin/parakeet-mlx-server" ];
    RunAtLoad = true;
    KeepAlive = true;
    ProcessType = "Interactive";
    StandardOutPath = "/tmp/parakeet-mlx-server.log";
    StandardErrorPath = "/tmp/parakeet-mlx-server.log";
  };
};
```

### Phase 2: whisper.el Integration via HTTP Server Mode

**Goal:** Make whisper.el use the persistent parakeet-mlx HTTP server instead of spawning a CLI process per transcription.

#### 2.1 Choose whisper.el's Server Mode

whisper.el has three transcription modes controlled by `whisper-server-mode`:
- `nil` — subprocess (calls `whisper-command`)
- `'local` — local server (calls `whisper--transcribe-via-local-server`)
- `'remote` — remote server (same, but doesn't try to start server)
- `'openai` — OpenAI API

The **remote** mode is perfect: we set `whisper-server-mode` to `'remote`, configure `whisper-server-host`/`whisper-server-port` to point at our parakeet-mlx server, and override `whisper--transcribe-via-local-server` to format the request as `/v1/audio/transcriptions`.

**But wait** — `whisper--transcribe-via-local-server` hardcodes the URL path as `"inference"` (whisper.cpp server endpoint). The parakeet-mlx server should expose `/v1/audio/transcriptions` (OpenAI-compatible), so we need to either:
- (a) Override `whisper--transcribe-via-local-server` to use our endpoint (as Sacha does)
- (b) Also expose an `/inference` endpoint for full whisper.cpp compatibility
- (c) Use `whisper-server-mode = 'openai` and set `whisper-openai-base-url` to our server

Actually, looking at whisper.el more carefully, `whisper--transcribe-via-local-server` sends to `(file-name-concat (whisper--server-base-url) "inference")`. And `whisper--server-base-url` returns `http://host:port`. So it sends to `http://127.0.0.1:5092/inference`.

**Recommendation:** Make parakeet-mlx-server serve both:
- `/v1/audio/transcriptions` — OpenAI-compatible (Sacha's pattern, future-proof)
- `/inference` — whisper.cpp format (for compatibility with unpatched whisper.el)

And we override `whisper--transcribe-via-local-server` just like Sacha does, pointing it to `/v1/audio/transcriptions`.

#### 2.2 Emacs Configuration Changes

In `modules/speak2text.nix`, the `whisper` use-package block currently does:

```elisp
(defun whisper-command (input-file)
  `("${transcribeBinPath}" ,input-file))
```

This only works for subprocess mode. When we have a server running, the better approach is:

```elisp
(setq whisper-server-mode 'remote
      whisper-server-host "127.0.0.1"
      whisper-server-port 5092)
(advice-add 'whisper--transcribe-via-local-server :override
            #'my-whisper--transcribe-via-parakeet)
(advice-add 'whisper--check-model-consistency :override
            (lambda () t))
```

Where `my-whisper--transcribe-via-parakeet` sends a multipart POST to `http://127.0.0.1:5092/v1/audio/transcriptions`.

**But this requires Emacs Lisp code in the config.** The cleanest approach:

Add a new option to speak2text.nix: `cfg.whisperMode` with values `"subprocess"` or `"server"`, and generate the appropriate Emacs config.

### Phase 3: Update speak2text.nix Flavor System

#### 3.1 Add `parakeet-mlx-server` Flavor Variant

The current `parakeet-mlx` flavor uses CLI invocation. Add a new flavor or mode:

```nix
flavor = lib.mkOption {
  type = lib.types.enum [
    "whispercpp"
    "parakeet-mlx"        # CLI (current)
    "parakeet-mlx-server" # HTTP server (new)
    "cohere-transcribe"
  ];
};
```

Or alternatively, add a separate boolean `cfg.useServer` that, when true for `parakeet-mlx`, starts the server and configures Emacs in server mode.

**Recommendation:** Keep it simple — add a boolean `cfg.parakeetServer` that only applies when `cfg.flavor == "parakeet-mlx"`. When true:
- Launch the `parakeet-mlx-server` launchd agent
- Configure Emacs whisper.el for server mode (overrides)
- The PTT listener and CLI `speak2text` also use HTTP instead of CLI

#### 3.2 Update PTT Listener for Server Mode

When `parakeetServer` is enabled, the PTT listener should POST to the server instead of spawning `parakeet-mlx` CLI. This is faster (no model load, no process spawn) and works identically to Sacha's approach.

Add to `speak2text-ptt-listener.py`:

```python
def _transcribe_via_server(wav_path: str, port: int = 5092) -> str:
    """POST to parakeet-mlx-server and return transcript text."""
    import urllib.request, urllib.error, json
    url = f"http://127.0.0.1:{port}/v1/audio/transcriptions"
    with open(wav_path, 'rb') as f:
        files = f.read()
    boundary = b'----WebKitFormBoundary7MA4YWxkTrZu0gW'
    body = (
        b'--' + boundary + b'\r\n'
        b'Content-Disposition: form-data; name="file"; filename="audio.wav"\r\n'
        b'Content-Type: audio/wav\r\n\r\n' + files + b'\r\n'
        b'--' + boundary + b'\r\n'
        b'Content-Disposition: form-data; name="response_format"\r\n\r\njson\r\n'
        b'--' + boundary + b'--\r\n'
    )
    req = urllib.request.Request(url, data=body,
        method='POST',
        headers={'Content-Type': f'multipart/form-data; boundary={boundary.decode()}'})
    with urllib.request.urlopen(req, timeout=30) as resp:
        return json.loads(resp.read()).get('text', '')
```

### Phase 4: Update CLI `speak2text` for Server Mode

When the server is running, the `speak2text` shell command should also try the server first, falling back to CLI:

```
If parakeet-mlx-server is up on port 5092:
  POST the WAV and get text back (~0.5-1s on M2 Max)
Else:
  Fall back to parakeet-mlx CLI (~2-5s, cold start each time)
```

### Phase 5: Streaming / Real-time Transcription

#### 5.1 Parakeet-MLX Streaming API

The parakeet-mlx library now has `transcribe_stream()` which supports real-time chunked input. This is exciting because it means we could do:

1. Start recording audio
2. Feed chunks to `transcribe_stream()` as they arrive
3. Display partial results in Emacs in real-time

This would require:
- A WebSocket or Server-Sent Events endpoint on parakeet-mlx-server
- Emacs client code that reads partial results and updates a buffer/mode-line

**This is a stretch goal** for later. The foundation (persistent server) must come first.

### Phase 6: Additional Integrations

#### 6.1 Replace macOS `say` with `mlx-speak` in `claude-voice`

Currently `claude-voice` uses `/usr/bin/say` for TTS output. The `mlx-speak-server` (already built!) could replace this for better-quality TTS. This is a simple change.

#### 6.2 Combine STT + TTX (TTS) into a Voice Assistant Server

A unified agent that:
- Listens (parakeet-mlx-server for STT)
- Reasons (Claude, local LLM via Ollama)
- Speaks (mlx-speak-server for TTS)

Both servers already exist in the packages! The `claude-voice` script currently does this linearly; a server version could be always-on and faster.

#### 6.3 Whisper-cpp Server Mode as Alternative

For completeness: `whisper-cpp` package already includes `whisper-server`. We could:
- Add a `whispercpp-server` flavor to speak2text.nix
- Run `whisper-server` as a launchd agent
- Point whisper.el at it in `'local` or `'remote` mode
- This would be faster than subprocess for repeated dictation

---

## Implementation Order

| Step | What | Effort | Impact |
|------|------|-------|--------|
| **1** | Create `packages/parakeet-mlx-server.nix` | Medium | **Core enabler** — eliminates 2–5s cold start |
| **2** | Add `parakeetServer` option to `modules/speak2text.nix` | Small | Wiring for launchd + Emacs |
| **3** | Write Emacs Elisp override for whisper.el server mode | Small | Interactive dictation via server |
| **4** | Update PTT listener to use HTTP server when available | Small | Faster push-to-talk |
| **5** | Update CLI `speak2text` to try server first | Small | Faster CLI use |
| **6** | Add `whispercpp-server` flavor | Small | Choice of backend |
| **7** | Streaming transcription (stretch) | Large | Real-time display |
| **8** | Unified STT+TTS voice assistant server | Medium | Always-on voice assistant |

---

## Detailed Implementation Notes for Step 1: `parakeet-mlx-server`

### Server Script (`parakeet-mlx-server`)

```python
#!/usr/bin/env python3
"""Persistent HTTP server for parakeet-mlx speech-to-text on Apple Silicon."""

import os
import sys
import json
import signal
import tempfile
import time
import mimetypes
import email.parser
from http.server import HTTPServer, BaseHTTPRequestHandler
from pathlib import Path

from parakeet_mlx import from_pretrained, DecodingConfig
from parakeet_mlx.audio import load_audio

PORT = int(os.environ.get("PARAKEET_MLX_PORT", "5092"))
MODEL_NAME = os.environ.get("PARAKEET_MODEL", "mlx-community/parakeet-tdt-0.6b-v3")

print(f"parakeet-mlx-server: Loading model {MODEL_NAME}...", file=sys.stderr, flush=True)
model = from_pretrained(MODEL_NAME)
print(f"parakeet-mlx-server: Model loaded. Warming up Metal JIT...", file=sys.stderr, flush=True)

# Warm up: transcribe 1 second of silence
import numpy as np
SAMPLE_RATE = model.preprocessor_config.sample_rate
silence = np.zeros(SAMPLE_RATE, dtype=np.float32)
result = model.transcribe(silence, decoding_config=DecodingConfig())
print(f"parakeet-mlx-server: Warmup done. Ready on 127.0.0.1:{PORT}", file=sys.stderr, flush=True)


def transcribe_file(audio_path: str) -> str:
    """Transcribe an audio file and return the text."""
    audio = load_audio(audio_path, model.preprocessor_config.sample_rate)
    result = model.transcribe(audio, decoding_config=DecodingConfig())
    return result.text


def parse_multipart(body: bytes, content_type: str) -> dict:
    """Parse multipart form data, returns dict of {name: (filename, bytes)}."""
    # Simple multipart parser
    fields = {}
    boundary = None
    for part in content_type.split(";"):
        part = part.strip()
        if part.startswith("boundary="):
            boundary = part[len("boundary="):].strip('"')
    if not boundary:
        return fields

    boundary_bytes = boundary.encode()
    delimiter = b"--" + boundary_bytes
    parts = body.split(delimiter)

    for part in parts[1:]:  # Skip preamble
        if part.strip() in (b"", b"--", b"--\r\n"):
            continue
        # Split headers from body
        if b"\r\n\r\n" not in part:
            continue
        header_section, body_data = part.split(b"\r\n\r\n", 1)
        # Remove trailing \r\n
        if body_data.endswith(b"\r\n"):
            body_data = body_data[:-2]

        # Parse Content-Disposition
        name = None
        filename = None
        for line in header_section.decode("utf-8", errors="replace").split("\r\n"):
            if line.lower().startswith("content-disposition:"):
                for item in line.split(";"):
                    item = item.strip()
                    if item.startswith("name="):
                        name = item[5:].strip('"')
                    elif item.startswith("filename="):
                        filename = item[9:].strip('"')
        if name:
            fields[name] = (filename, body_data)

    return fields


class ParakeetHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        content_length = int(self.headers.get("Content-Length", 0))
        content_type = self.headers.get("Content-Type", "")
        body = self.rfile.read(content_length)

        t0 = time.time()

        if self.path in ("/v1/audio/transcriptions", "/transcribe", "/inference"):
            try:
                if "multipart" in content_type:
                    fields = parse_multipart(body, content_type)
                    file_data = None
                    for name, (filename, data) in fields.items():
                        if name == "file" or (filename and filename.endswith((".wav", ".mp3", ".flac", ".ogg", ".m4a"))):
                            file_data = (filename or name, data)
                            break
                    if not file_data:
                        self.send_error(400, "No audio file found in request")
                        return
                    fname, audio_bytes = file_data
                else:
                    # Raw audio body
                    audio_bytes = body
                    fname = "audio.wav"

                # Write to temp file
                suffix = Path(fname).suffix if fname else ".wav"
                with tempfile.NamedTemporaryFile(suffix=suffix, delete=False) as f:
                    f.write(audio_bytes)
                    tmp_path = f.name

                try:
                    text = transcribe_file(tmp_path)
                finally:
                    os.unlink(tmp_path)

                elapsed = time.time() - t0
                print(f"parakeet-mlx-server: {self.path} transcribed {len(audio_bytes)}B audio in {elapsed:.2f}s: {text!r}",
                      file=sys.stderr, flush=True)

                # Return OpenAI-compatible response
                if self.path == "/v1/audio/transcriptions":
                    response = json.dumps({"text": text})
                    self.send_response(200)
                    self.send_header("Content-Type", "application/json")
                    self.end_headers()
                    self.wfile.write(response.encode())
                else:
                    # /transcribe or /inference: return plain text
                    self.send_response(200)
                    self.send_header("Content-Type", "text/plain; charset=utf-8")
                    self.end_headers()
                    self.wfile.write(text.encode())

            except Exception as e:
                print(f"parakeet-mlx-server ERROR: {e}", file=sys.stderr, flush=True)
                import traceback
                traceback.print_exc(file=sys.stderr)
                self.send_error(500, str(e))
        else:
            self.send_error(404)

    def do_GET(self):
        if self.path in ("/", "/health"):
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps({"status": "ok", "model": MODEL_NAME}).encode())
        else:
            self.send_error(404)

    def log_message(self, format, *args):
        pass  # Suppress per-request noise; we log in handler


if __name__ == "__main__":
    server = HTTPServer(("127.0.0.1", PORT), ParakeetHandler)
    server.serve_forever()
```

### Nix Package

```nix
# packages/parakeet-mlx-server.nix
{ writers, parakeet-mlx }:

writers.writePython3Bin "parakeet-mlx-server" {
  libraries = [ parakeet-mlx ];
  flakeIgnore = [ "E501" "E402" ];
} ./parakeet-mlx-server.py
```

---

## Emacs Configuration for Server Mode

```elisp
;; When speak2text flavor is "parakeet-mlx" and parakeetServer is true:
(setq whisper-server-mode 'remote
      whisper-server-host "127.0.0.1"
      whisper-server-port 5092)

(defun my-whisper--transcribe-via-parakeet-server ()
  "Transcribe audio using the parakeet-mlx HTTP server."
  (message "[-] Transcribing via parakeet-mlx server")
  (whisper--setup-mode-line :show 'transcribing)
  (setq whisper--transcribing-process
        (whisper--process-curl-request
         (format "http://%s:%d/v1/audio/transcriptions"
                 whisper-server-host whisper-server-port)
         (list "Content-Type: multipart/form-data")
         (list (concat "file=@" whisper--temp-file)
               "temperature=0.0"
               "temperature_inc=0.2"
               "response_format=json"
               (concat "language=" whisper-language)))))

(defun my-whisper--check-model-consistency () t)

(with-eval-after-load 'whisper
  (advice-add 'whisper--transcribe-via-local-server :override
              #'my-whisper--transcribe-via-parakeet-server)
  (advice-add 'whisper--check-model-consistency :override
              #'my-whisper--check-model-consistency))
```

**Note:** `whisper--process-curl-request` is an internal function in whisper.el that uses `curl` to send multipart form data. This is exactly what Sacha Chua uses. However, looking at the actual whisper.el code, `whisper--transcribe-via-local-server` sends to the `"inference"` endpoint with specific form fields. Our server needs to handle either:
- The exact fields whisper.cpp server expects (`file`, `temperature`, etc.), OR
- The OpenAI-compatible `/v1/audio/transcriptions` format

Since we're overriding `whisper--transcribe-via-local-server`, we control the URL and format. We'll use `/v1/audio/transcriptions` with OpenAI-compatible response format.

### Alternative: Use whisper-server-mode = nil (subprocess with server-aware wrapper)

If we don't want to advise whisper.el internals, we could create a thin CLI wrapper that POSTs to the parakeet server:

```bash
#!/bin/sh
# speak2text-transcribe-via-parakeet-server — posts WAV to local HTTP server
curl -s -X POST http://127.0.0.1:5092/v1/audio/transcriptions \
  -F "file=@$1" -F "response_format=json" | python3 -c "import json,sys; print(json.load(sys.stdin)['text'])"
```

And set `(setq whisper-command (lambda (input-file) `("parakeet-server-client" ,input-file)))`.

**Recommendation:** Use the advice approach (Sacha's pattern). It gives us the benefits of whisper.el's progress tracking, mode-line indicators, and `whisper-after-transcription-hook` while using our fast server.

---

## Summary of Files to Create/Modify

### New Files
| File | Purpose |
|------|---------|
| `packages/parakeet-mlx-server.py` | Python server script |
| `packages/parakeet-mlx-server.nix` | Nix packaging for the server |

### Modified Files
| File | Changes |
|------|----------|
| `modules/speak2text.nix` | Add `parakeetServer` bool option, launchd agent, Emacs server-mode config, update PTT listener to use HTTP |
| `modules/speak2text-ptt-listener.py` | Add `_transcribe_via_server()` fallback |
| `packages/flake.nix` | Add `parakeet-mlx-server` package |
| `modules/editor/emacs/emacs-init-defaults.nix` | Optionally coordinate whisper.el config with speak2text module |
| `packages/default.nix` | Expose `parakeet-mlx-server` |

### No Changes Needed
| Component | Reason |
|-----------|--------|
| `packages/parakeet-mlx.nix` | Already works; server imports it as a library |
| `packages/parakeet-transcribe.nix` | CLI tool still useful as fallback |
| `packages/claude-voice.nix` | Can optionally switch to HTTP later |
| `packages/mlx-speak*.nix` | TTS (separate concern, already works) |