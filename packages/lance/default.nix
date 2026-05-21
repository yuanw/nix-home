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
    hash = "sha256-2fkog+feEJDcq5LRjjeAIy04LNUbkpWcGOgVPKRT9U4=";
  };

  # Python environment with all Lance dependencies.
  lancePython = python3.withPackages (
    ps: with ps; [
      torch
      torchvision
      (torchaudio.overridePythonAttrs (_old: {
        doCheck = false;
      }))
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
      pkgs.decord
      ((builtins.getAttr "flash-attn" ps).overridePythonAttrs (old: {
        meta = (old.meta or { }) // {
          broken = false;
        };
        # CUDA 12.9 requires g++ < 15.0; use gcc14 for compatibility
        stdenv = pkgs.gcc14Stdenv;
        # Reduce parallelism to avoid OOM on DGX Spark (unified memory, GPU uses most RAM)
        preConfigure = (old.preConfigure or "") + ''
          export MAX_JOBS=1
          export NVCC_THREADS=2
        '';
      }))
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

    # Patch ValidationDataset import to be lazy (avoids missing decord at startup)
    # Move top-level import to just before first use
    substituteInPlace $out/share/lance/lance_gradio_t2v_v2t.py \
      --replace 'from data.datasets_custom import ValidationDataset' \
                '# from data.datasets_custom import ValidationDataset  # moved inside function'
    substituteInPlace $out/share/lance/lance_gradio_t2v_v2t.py \
      --replace '        val_dataset = ValidationDataset(' \
                '        from data.datasets_custom import ValidationDataset; val_dataset = ValidationDataset('

    # Same lazy import fix for inference_lance.py (also imports ValidationDataset)
    substituteInPlace $out/share/lance/inference_lance.py \
      --replace 'from data.datasets_custom import ValidationDataset' \
                '# from data.datasets_custom import ValidationDataset  # moved inside function'
    substituteInPlace $out/share/lance/inference_lance.py \
      --replace '    val_dataset = ValidationDataset(' \
                '    from data.datasets_custom import ValidationDataset; val_dataset = ValidationDataset('

    mkdir -p $out/share/lance/downloads
    mkdir -p $out/share/lance/results

    runHook postBuild
  '';

  installPhase = ''
        runHook preInstall

        mkdir -p $out/bin

        # ── 1. lance-gradio ──
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
          # Make writable (nix store files are read-only)
          chmod -R u+w "$LANCE_DATA_DIR"
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
            if command -v lance-download-model >/dev/null 2>&1; then
              exec lance-download-model "$@"
            else
              echo "ERROR: lance-download-model not found. Install the lance-download-model package."
              exit 1
            fi
            ;;
          *)
            usage
            ;;
        esac
    BINSH

        # ── 4. Substitute store paths ──
        substituteInPlace $out/bin/lance-gradio \
          --replace-fail '__PYTHON_EXEC__' "${pythonExec}" \
          --replace-fail '__LANCE_SHARE__' "$out/share/lance"

        chmod +x $out/bin/lance
        chmod +x $out/bin/lance-gradio

        # Fix shebang lines: remove any leading whitespace before #!
        # (Nix indented strings can leave partial indentation)
        sed -i '1s/^[[:space:]]\{1,\}#!/#!/' $out/bin/lance-gradio
        sed -i '1s/^[[:space:]]\{1,\}#!/#!/' $out/bin/lance

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
