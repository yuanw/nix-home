{
  lib,
  pkgs,
  fetchFromGitHub,
  python3,
  ...
}:

let
  version = "0-unstable-2026-05-19";
  src = fetchFromGitHub {
    owner = "bytedance";
    repo = "Lance";
    rev = "main";
    hash = ""; # FIXME: set after first build
  };

  # Python environment with all Lance dependencies.
  lancePython = python3.withPackages (
    ps: with ps; [
      torch
      torchvision
      torchaudio
      transformers
      diffusers
      accelerate
      einops
      safetensors
      sentencepiece
      tiktoken
      tokenizers
      pillow
      numpy
      scipy
      scikit-image
      click
      h5py
      imageio
      imageio-ffmpeg
      opencv-python
      gradio
      psutil
      tqdm
      requests
      huggingface-hub
      jinja2
      pyyaml
      omegaconf
      addict
      tabulate
      tenacity
      termcolor
      filelock
      fsspec
      certifi
      cffi
      charset-normalizer
      protobuf
      pydantic
      typing-extensions
      packaging
      kornia
      librosa
      soundfile
      gpustat
      httpx
      peft
      timm
      webdataset
      yacs
      ftfy
      joblib
      albumentations
      torchmetrics
      triton
    ]
  );

  pythonExec = "${lancePython}/bin/python3";
in

pkgs.stdenv.mkDerivation {
  pname = "lance";
  inherit version;
  inherit src;

  strictDeps = true;

  nativeBuildInputs = [
    lancePython
  ];

  buildInputs = [
    lancePython
    pkgs.bash
    pkgs.coreutils
  ];

  buildPhase = ''
    runHook preBuild

    mkdir -p $out/share/lance
    mkdir -p $out/share/lance/tmps/gradio_t2v_v2t

    cp -r modeling          $out/share/lance/
    cp -r common            $out/share/lance/
    cp -r config            $out/share/lance/
    cp -r data              $out/share/lance/
    cp -r benchmarks        $out/share/lance/
    cp inference_lance.py   $out/share/lance/
    cp inference_lance.sh   $out/share/lance/
    cp lance_gradio_t2v_v2t.py $out/share/lance/
    cp setup_env.sh         $out/share/lance/
    cp requirements.txt     $out/share/lance/

    mkdir -p $out/share/lance/downloads
    mkdir -p $out/share/lance/results

    runHook postBuild
  '';

  installPhase = ''
        runHook preInstall

        mkdir -p $out/bin

        # ── 1. lance-download-model ──
        cat > $out/bin/lance-download-model << 'BINSH'
        #!/bin/sh
        set -e
        : "''${LANCE_DATA_DIR:=$PWD}"
        : "''${HF_TOKEN:=}"

        MODELS_DIR="$LANCE_DATA_DIR/downloads"
        mkdir -p "$MODELS_DIR"

        echo "=== Lance Model Downloader ==="
        echo "Models directory: $MODELS_DIR"
        echo ""

        download_variant() {
          VARIANT="$1"
          DIR="$MODELS_DIR/$VARIANT"
          mkdir -p "$DIR"
          echo "Downloading $VARIANT ..."

          __python3__ -c "
        import os, sys
        from huggingface_hub import snapshot_download, hf_hub_download

        repo_id = 'bytedance-research/Lance'
        variant = '$VARIANT'
        dest = '$DIR'

        snapshot_download(
            repo_id=repo_id,
            allow_patterns=[f'{variant}/*'],
            local_dir=dest,
            local_dir_use_symlinks=False,
        )
        print(f'Downloaded {variant} to {dest}')
        "
        }

        download_variant "Lance_3B"
        download_variant "Lance_3B_Video"

        echo "Downloading shared components ..."
        __python3__ -c "
        from huggingface_hub import snapshot_download, hf_hub_download
        repo_id = 'bytedance-research/Lance'

        snapshot_download(
            repo_id=repo_id,
            allow_patterns=['Qwen2.5-VL-ViT/*'],
            local_dir='$MODELS_DIR',
            local_dir_use_symlinks=False,
        )

        hf_hub_download(
            repo_id=repo_id,
            filename='Wan2.2_VAE.pth',
            local_dir='$MODELS_DIR',
            local_dir_use_symlinks=False,
        )
        print('Shared components downloaded.')
        "

        echo ""
        echo "=== Download complete ==="
        echo "Models are at: $MODELS_DIR"
        ls -la "$MODELS_DIR"
    BINSH

        # ── 2. lance-gradio ──
        cat > $out/bin/lance-gradio << 'BINSH'
        #!/bin/sh
        set -e

        : "''${LANCE_DATA_DIR:=/var/lib/lance}"
        : "''${MODEL_PATH:=$LANCE_DATA_DIR/downloads/Lance_3B_Video}"
        : "''${SERVER_PORT:=7860}"
        : "''${SERVER_NAME:=0.0.0.0}"
        : "''${GPU_DEVICE:=0}"
        : "''${QUEUE_SIZE:=32}"
        : "''${POSITION_EMBEDDING_3D_VERSION:=v2}"
        : "''${CUDA_LAUNCH_BLOCKING:=0}"
        : "''${NCCL_DEBUG:=VERSION}"

        export POSITION_EMBEDDING_3D_VERSION
        export CUDA_LAUNCH_BLOCKING
        export NCCL_DEBUG

        mkdir -p "$LANCE_DATA_DIR"
        if [ ! -d "$LANCE_DATA_DIR/common" ]; then
          echo "Initialising Lance app directory in $LANCE_DATA_DIR ..."
          cp -r __LANCE_SHARE__/* "$LANCE_DATA_DIR/"
          mkdir -p "$LANCE_DATA_DIR/downloads"
          mkdir -p "$LANCE_DATA_DIR/results"
        fi

        if [ -d "$MODEL_PATH" ]; then
          mkdir -p "$LANCE_DATA_DIR/downloads"
          ln -sfn "$MODEL_PATH" "$LANCE_DATA_DIR/downloads/Lance_3B_Video"
        fi

        cd "$LANCE_DATA_DIR"
        echo "Starting Lance Gradio server (port=$SERVER_PORT, gpu=$GPU_DEVICE) ..."
        echo "Model path: $MODEL_PATH"

        exec __PYTHON_EXEC__ lance_gradio_t2v_v2t.py \
          --gpus "$GPU_DEVICE" \
          --server-name "$SERVER_NAME" \
          --server-port "$SERVER_PORT" \
          --queue-size "$QUEUE_SIZE"
    BINSH

        # ── 3. lance CLI ──
        cat > $out/bin/lance << 'BINSH'
        #!/bin/sh
        set -e
        : "''${LANCE_DATA_DIR:=/var/lib/lance}"

        usage() {
          cat <<EOF
        Lance Multimodal AI CLI

        Tasks (auto-selects model variant):
          lance t2v --prompt "..." --output result.mp4
          lance t2i --prompt "..." --output result.png
          lance image-edit --prompt "..." --image input.jpg --output result.png
          lance video-edit --prompt "..." --video input.mp4 --output result.mp4
          lance understand --image photo.jpg --question "What is this?"

        Server:
          lance serve [port] [task]

        Model management:
          lance download-models
    EOF
          exit 1
        }

        [ $# -eq 0 ] && usage

        CMD="$1"
        shift

        case "$CMD" in
          t2v|t2i|image-edit|video-edit|understand)
            PROMPT="" OUTPUT="" IMAGE="" VIDEO="" QUESTION=""
            TIMESTEPS="''${VALIDATION_NUM_TIMESTEPS:-30}"
            TIMESTEP_SHIFT="''${VALIDATION_TIMESTEP_SHIFT:-3.5}"
            CFG_SCALE="''${CFG_TEXT_SCALE:-4.0}"
            SEED="''${VALIDATION_DATA_SEED:-42}"
            RESOLUTION="''${RESOLUTION:-video_480p}"
            NUM_FRAMES="''${NUM_FRAMES:-50}"
            HEIGHT="''${VIDEO_HEIGHT:-480}"
            WIDTH="''${VIDEO_WIDTH:-848}"

            while [ $# -gt 0 ]; do
              case "$1" in
                --prompt)   PROMPT="$2";  shift 2  ;;
                --output)   OUTPUT="$2";  shift 2  ;;
                --image)    IMAGE="$2";   shift 2  ;;
                --video)    VIDEO="$2";   shift 2  ;;
                --question) QUESTION="$2"; shift 2 ;;
                --timesteps) TIMESTEPS="$2";  shift 2 ;;
                --cfg-scale) CFG_SCALE="$2";  shift 2 ;;
                --seed)     SEED="$2";    shift 2  ;;
                --resolution) RESOLUTION="$2"; shift 2 ;;
                --num-frames) NUM_FRAMES="$2"; shift 2 ;;
                --height)   HEIGHT="$2";  shift 2  ;;
                --width)    WIDTH="$2";   shift 2  ;;
                *) echo "Unknown arg: $1"; usage ;;
              esac
            done

            TASK="$CMD"
            case "$CMD" in
              t2v)         TASK="t2v"       ;;
              t2i)         TASK="t2i"       ;;
              image-edit)  TASK="image_edit" ;;
              video-edit)  TASK="video_edit" ;;
              understand)  TASK="x2t_video"  ;;
            esac

            # Auto-select model variant based on task
            case "$TASK" in
              t2i|image_edit|x2t_image)
                MODEL_PATH="''${MODEL_PATH_IMAGE:-$LANCE_DATA_DIR/downloads/Lance_3B}"
                ;;
              *)
                MODEL_PATH="''${MODEL_PATH:-$LANCE_DATA_DIR/downloads/Lance_3B_Video}"
                ;;
            esac

            cd "$LANCE_DATA_DIR"
            TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
            SAVE_PATH="results/''${TASK}_sample_''${TIMESTAMP}"
            mkdir -p "$SAVE_PATH"

            echo "Lance $TASK (model: $(basename "$MODEL_PATH"))"

            exec accelerate launch \
              --num_machines 1 \
              --num_processes 1 \
              --mixed_precision bf16 \
              inference_lance.py \
              --model_path "$MODEL_PATH" \
              --vit_type qwen_2_5_vl_original \
              --llm_qk_norm true \
              --llm_qk_norm_und true \
              --llm_qk_norm_gen true \
              --tie_word_embeddings false \
              --validation_num_timesteps "$TIMESTEPS" \
              --validation_timestep_shift "$TIMESTEP_SHIFT" \
              --copy_init_moe true \
              --max_num_frames 121 \
              --max_latent_size 64 \
              --latent_patch_size 1 1 1 \
              --visual_und true \
              --visual_gen true \
              --vae_model_type wan \
              --apply_qwen_2_5_vl_pos_emb true \
              --apply_chat_template false \
              --cfg_type 0 \
              --validation_data_seed "$SEED" \
              --video_height "$HEIGHT" \
              --video_width "$WIDTH" \
              --num_frames "$NUM_FRAMES" \
              --task "$TASK" \
              --save_path_gen "$SAVE_PATH" \
              --resolution "$RESOLUTION" \
              --text_template true \
              --cfg_text_scale "$CFG_SCALE" \
              --use_KVcache true
            ;;
          serve)
            exec $out/bin/lance-gradio "$@"
            ;;
          download-models)
            exec $out/bin/lance-download-model "$@"
            ;;
          *)
            usage
            ;;
        esac
    BINSH

        # ── 4. Substitute store paths ──
        for f in $out/bin/lance-gradio $out/bin/lance $out/bin/lance-download-model; do
          substituteInPlace "$f" \
            --replace-fail '__PYTHON_EXEC__' "${pythonExec}" \
            --replace-fail '__LANCE_SHARE__' "$out/share/lance"
        done
        substituteInPlace $out/bin/lance-download-model \
          --replace-fail '__python3__' "${pythonExec}"

        chmod +x $out/bin/lance
        chmod +x $out/bin/lance-gradio
        chmod +x $out/bin/lance-download-model

        runHook postInstall
  '';

  meta = {
    description = "Lance 3B multimodal AI model – Gradio server, CLI, and model downloader";
    homepage = "https://github.com/bytedance/Lance";
    license = lib.licenses.mit;
    platforms = [
      "aarch64-linux"
      "x86_64-linux"
    ];
    mainProgram = "lance";
  };
}
