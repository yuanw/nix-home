"""Persistent HTTP server for parakeet-mlx speech-to-text on Apple Silicon."""

import os
import sys
import json
import tempfile
import time
from http.server import HTTPServer, BaseHTTPRequestHandler
from pathlib import Path

from parakeet_mlx import from_pretrained, DecodingConfig

PORT = int(os.environ.get("PARAKEET_MLX_PORT", "5092"))
MODEL_NAME = os.environ.get("PARAKEET_MODEL", "mlx-community/parakeet-tdt-0.6b-v3")

print(f"parakeet-mlx-server: Loading model {MODEL_NAME}...", file=sys.stderr, flush=True)
model = from_pretrained(MODEL_NAME)
print("parakeet-mlx-server: Model loaded. Warming up Metal JIT...", file=sys.stderr, flush=True)

# Warm up: transcribe a tiny silent WAV file to compile Metal shaders
import wave

SAMPLE_RATE = model.preprocessor_config.sample_rate
_warmup_path = os.path.join(tempfile.gettempdir(), "parakeet-mlx-server-warmup.wav")
with wave.open(_warmup_path, "w") as wf:
    wf.setnchannels(1)
    wf.setsampwidth(2)  # 16-bit
    wf.setframerate(SAMPLE_RATE)
    wf.writeframes(b"\x00\x00" * SAMPLE_RATE)  # 1s of silence
result = model.transcribe(_warmup_path, decoding_config=DecodingConfig())
os.unlink(_warmup_path)
print(
    f"parakeet-mlx-server: Warmup done. Ready on 127.0.0.1:{PORT}",
    file=sys.stderr,
    flush=True,
)


def transcribe_file(audio_path: str) -> str:
    """Transcribe an audio file and return the text."""
    result = model.transcribe(audio_path, decoding_config=DecodingConfig())
    return result.text


def parse_multipart(body: bytes, content_type: str) -> dict:
    """Parse multipart form data, returns dict of {name: (filename, bytes)}."""
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
                        # Accept field named "file" (OpenAI/whisper.cpp standard)
                        # or any field whose uploaded filename looks like audio
                        if name == "file" or name == "audio" or (
                            filename is not None
                            and filename.endswith(
                                (".wav", ".mp3", ".flac", ".ogg", ".m4a")
                            )
                        ):
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
                print(
                    f"parakeet-mlx-server: {self.path} transcribed "
                    f"{len(audio_bytes)}B audio in {elapsed:.2f}s: {text!r}",
                    file=sys.stderr,
                    flush=True,
                )

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
                print(
                    f"parakeet-mlx-server ERROR: {e}", file=sys.stderr, flush=True
                )
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
            self.wfile.write(
                json.dumps({"status": "ok", "model": MODEL_NAME}).encode()
            )
        else:
            self.send_error(404)

    def log_message(self, format, *args):
        pass  # Suppress per-request noise; we log in handler


if __name__ == "__main__":
    server = HTTPServer(("127.0.0.1", PORT), ParakeetHandler)
    server.serve_forever()
