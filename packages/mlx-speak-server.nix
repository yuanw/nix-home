{
  writers,
  mlx-speech,
}:

writers.writePython3Bin "mlx-speak-server"
  {
    libraries = [ mlx-speech ];
    flakeIgnore = [ "E501" ];
  }
  ''
    import os
    import sys
    import tempfile
    import subprocess
    from pathlib import Path
    from http.server import HTTPServer, BaseHTTPRequestHandler

    import re
    import time
    import mlx.core as mx
    from mlx_speech.audio import write_wav
    from mlx_speech.checkpoints.layout import get_openmoss_v0_layouts
    from mlx_speech.generation import (
        MossTTSLocalGenerationConfig,
        synthesize_moss_tts_local_conversations,
    )
    from mlx_speech.models.moss_audio_tokenizer import load_moss_audio_tokenizer_model
    from mlx_speech.models.moss_local import MossTTSLocalProcessor, load_moss_tts_local_model

    models_root = Path(
        os.environ.get("MLX_SPEECH_MODELS", Path.home() / ".local/share/mlx-speech/models")
    )
    layout = get_openmoss_v0_layouts(models_root=models_root)

    print("mlx-speak-server: loading TTS model...", file=sys.stderr, flush=True)
    loaded_model = load_moss_tts_local_model(str(layout.moss_tts_local.mlx_int8_dir))
    loaded_codec = load_moss_audio_tokenizer_model(str(layout.audio_tokenizer.mlx_int8_dir))
    processor = MossTTSLocalProcessor.from_path(
        loaded_model.model_dir,
        audio_tokenizer=loaded_codec.model,
    )
    config = MossTTSLocalGenerationConfig()

    # Warm up Metal JIT before binding the port — the first synthesis compiles
    # Metal shaders; doing it here means real requests are fast from the start
    print("mlx-speak-server: warming up Metal JIT...", file=sys.stderr, flush=True)
    _t = time.time()
    synthesize_moss_tts_local_conversations(
        loaded_model.model,
        processor,
        loaded_codec.model,
        conversations=[[processor.build_user_message(text="Hello.")]],
        mode="generation",
        config=config,
    )
    print(f"mlx-speak-server: warmup done ({time.time() - _t:.1f}s)", file=sys.stderr, flush=True)


    class SpeakHandler(BaseHTTPRequestHandler):
        def do_POST(self):
            if self.path != "/speak":
                self.send_response(404)
                self.end_headers()
                return
            length = int(self.headers.get("Content-Length", 0))
            text = self.rfile.read(length).decode("utf-8").strip()
            t_request = time.time()
            print(f"mlx-speak-server: request received ({length} bytes)", file=sys.stderr, flush=True)
            try:
                if text:
                    # Split on sentence boundaries; merge fragments shorter than
                    # 15 chars into the next sentence (very short inputs can
                    # produce degenerate output from the TTS model)
                    raw = [s.strip() for s in re.split(r"(?<=[.!?;])\s+", text) if s.strip()]
                    sentences: list[str] = []
                    carry = ""
                    for s in raw:
                        carry = (carry + " " + s).strip() if carry else s
                        if len(carry) >= 15:
                            sentences.append(carry)
                            carry = ""
                    if carry:
                        if sentences:
                            sentences[-1] += " " + carry
                        else:
                            sentences.append(carry)
                    if not sentences:
                        sentences = [text]
                    print(f"mlx-speak-server: {len(sentences)} sentence(s), {len(text)} chars total", file=sys.stderr, flush=True)
                    for i, sentence in enumerate(sentences):
                        t0 = time.time()
                        print(f"mlx-speak-server: [{i + 1}/{len(sentences)}] synthesizing {len(sentence)} chars: {sentence!r}", file=sys.stderr, flush=True)
                        conversations = [[processor.build_user_message(text=sentence)]]
                        result = synthesize_moss_tts_local_conversations(
                            loaded_model.model,
                            processor,
                            loaded_codec.model,
                            conversations=conversations,
                            mode="generation",
                            config=config,
                        )
                        t_synth = time.time()
                        synthesis = result.outputs[0]
                        with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as f:
                            out_path = Path(f.name)
                        write_wav(out_path, synthesis.waveform, sample_rate=synthesis.sample_rate)
                        wav_size = out_path.stat().st_size
                        print(f"mlx-speak-server: [{i + 1}/{len(sentences)}] synth={t_synth - t0:.1f}s wav={wav_size}B playing...", file=sys.stderr, flush=True)
                        subprocess.run(["/usr/bin/afplay", str(out_path)])
                        out_path.unlink()
                        t_play = time.time()
                        print(f"mlx-speak-server: [{i + 1}/{len(sentences)}] play={t_play - t_synth:.1f}s total={t_play - t0:.1f}s", file=sys.stderr, flush=True)
                        # Free Metal cache between sentences to avoid OOM on long responses
                        mx.metal.clear_cache()
                    print(f"mlx-speak-server: done total={time.time() - t_request:.1f}s", file=sys.stderr, flush=True)
                self.send_response(200)
                self.end_headers()
            except Exception as e:
                import traceback
                print(f"mlx-speak-server ERROR: {e}", file=sys.stderr, flush=True)
                traceback.print_exc(file=sys.stderr)
                self.send_response(500)
                self.end_headers()

        def log_message(self, format, *args):
            pass  # suppress per-request noise


    port = int(os.environ.get("MLX_SPEAK_PORT", "47732"))
    print(f"mlx-speak-server: ready on 127.0.0.1:{port}", file=sys.stderr, flush=True)
    HTTPServer(("127.0.0.1", port), SpeakHandler).serve_forever()
  ''
