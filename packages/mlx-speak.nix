{
  writers,
  mlx-speech,
}:

writers.writePython3Bin "mlx-speak"
  {
    libraries = [ mlx-speech ];
    flakeIgnore = [
      "E501"
      "E402"
    ];
  }
  ''
    import os
    import sys
    import urllib.request
    import urllib.error

    text = " ".join(sys.argv[1:]) if len(sys.argv) > 1 else sys.stdin.read().strip()
    if not text:
        sys.exit(0)

    port = int(os.environ.get("MLX_SPEAK_PORT", "47732"))

    # Try the server first — model already loaded, no startup cost
    try:
        req = urllib.request.Request(
            f"http://127.0.0.1:{port}/speak",
            data=text.encode("utf-8"),
            method="POST",
            headers={"Content-Type": "text/plain"},
        )
        with urllib.request.urlopen(req, timeout=120) as resp:
            resp.read()
        print(f"mlx-speak: used server on port {port}", file=sys.stderr, flush=True)
        sys.exit(0)
    except urllib.error.HTTPError as e:
        # Server running but synthesis failed — surface the error, don't fall back silently
        print(f"mlx-speak-server returned HTTP {e.code}: check /tmp/mlx-speak-server.log", file=sys.stderr)
        sys.exit(1)
    except (urllib.error.URLError, OSError):
        print(f"mlx-speak: server not available on port {port}, loading model locally...", file=sys.stderr, flush=True)

    # Fall back: load model inline (slow first call, but works standalone)
    import tempfile
    import subprocess
    from pathlib import Path

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

    loaded_model = load_moss_tts_local_model(str(layout.moss_tts_local.mlx_int8_dir))
    loaded_codec = load_moss_audio_tokenizer_model(str(layout.audio_tokenizer.mlx_int8_dir))
    processor = MossTTSLocalProcessor.from_path(
        loaded_model.model_dir,
        audio_tokenizer=loaded_codec.model,
    )

    config = MossTTSLocalGenerationConfig()
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
  ''
