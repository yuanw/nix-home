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

    class SpeakHandler(BaseHTTPRequestHandler):
        def do_POST(self):
            if self.path == "/speak":
                length = int(self.headers.get("Content-Length", 0))
                text = self.rfile.read(length).decode("utf-8").strip()
                if text:
                    conversations = [[processor.build_user_message(text=text)]]
                    result = synthesize_moss_tts_local_conversations(
                        loaded_model.model,
                        processor,
                        loaded_codec.model,
                        conversations=conversations,
                        mode="generation",
                        config=config,
                    )
                    synthesis = result.outputs[0]
                    with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as f:
                        out_path = Path(f.name)
                    write_wav(out_path, synthesis.waveform, sample_rate=synthesis.sample_rate)
                    subprocess.run(["/usr/bin/afplay", str(out_path)], check=True)
                    out_path.unlink()
                self.send_response(200)
                self.end_headers()

        def log_message(self, format, *args):
            pass  # suppress per-request noise

    port = int(os.environ.get("MLX_SPEAK_PORT", "47732"))
    print(f"mlx-speak-server: ready on 127.0.0.1:{port}", file=sys.stderr, flush=True)
    HTTPServer(("127.0.0.1", port), SpeakHandler).serve_forever()
  ''
