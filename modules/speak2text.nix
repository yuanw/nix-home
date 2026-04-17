{
  config,
  lib,
  pkgs,
  isDarwin ? false,
  ...
}:
let
  cfg = config.modules.speak2text;

  pythonWithHf = pkgs.python3.withPackages (p: [ p.huggingface-hub ]);

  parakeet-mlx-init = pkgs.writeShellApplication {
    name = "parakeet-mlx-init";
    runtimeInputs = [ pythonWithHf ];
    text = ''
      echo "Downloading mlx-community/parakeet-tdt-0.6b-v3 to HuggingFace cache..." >&2
      python3 -c "
      from huggingface_hub import snapshot_download
      path = snapshot_download('mlx-community/parakeet-tdt-0.6b-v3')
      print(f'Model cached at: {path}')
      "
    '';
  };

  cohere-transcribe-init = pkgs.writeShellApplication {
    name = "cohere-transcribe-init";
    runtimeInputs = [ pythonWithHf ];
    text = ''
      MODEL_DIR="''${COHERE_TRANSCRIBE_MODEL_DIR:-''${HOME}/.local/share/cohere-transcribe/models/cohere-transcribe-03-2026}"
      echo "Note: CohereLabs/cohere-transcribe-03-2026 is an access-controlled model." >&2
      echo "Run 'huggingface-cli login' first if you haven't already." >&2
      echo "Downloading to ''${MODEL_DIR} ..." >&2
      huggingface-cli download CohereLabs/cohere-transcribe-03-2026 \
        --local-dir "''${MODEL_DIR}"
      echo "Copying vocab.json from Nix package..." >&2
      cp ${pkgs.cohere-transcribe}/share/cohere-transcribe/vocab.json "''${MODEL_DIR}/"
      echo "Done. Model ready at ''${MODEL_DIR}" >&2
    '';
  };

  # Server-aware transcribe script: tries HTTP server first, falls back to CLI
  parakeetMlxServerTranscribe =
    let
      port = toString cfg.parakeetServerPort;
    in
    pkgs.writeShellApplication {
      name = "speak2text-transcribe-wav-parakeet-mlx-server";
      runtimeInputs = [
        pkgs.coreutils
        pkgs.curl
        pkgs.python3
        pkgs.parakeet-mlx
      ];
      text = ''
        set -euo pipefail
        TMPFILE="''${1:?usage: speak2text-transcribe-wav FILE.wav}"
        step() { printf '[speak2text-transcribe] %s\n' "$(date +%H:%M:%S) $*" >&2; }
        TRANSCRIPT=""

        # Try server first
        if TRANSCRIPT=$(curl -sf http://127.0.0.1:${port}/v1/audio/transcriptions \
            -F "file=@$TMPFILE" 2>/dev/null \
            | python3 -c "import json,sys; print(json.load(sys.stdin)['text'])" 2>/dev/null) \
            && [ -n "$TRANSCRIPT" ]; then
          step "Server transcription succeeded"
        else
          # Fall back to CLI
          step "Server not available, falling back to parakeet-mlx CLI..."
          REAL_TMPFILE=$(realpath "$TMPFILE")
          TXT_OUT="''${REAL_TMPFILE%.wav}.txt"
          ( cd "$(dirname "$REAL_TMPFILE")" && parakeet-mlx --output-format txt "$REAL_TMPFILE" ) \
            && PARAKEET_EXIT=0 || PARAKEET_EXIT=$?
          if [[ $PARAKEET_EXIT -ne 0 ]]; then
            step "parakeet-mlx exited with code $PARAKEET_EXIT"
          fi
          TRANSCRIPT=$(cat "$TXT_OUT" 2>/dev/null || true)
          rm -f "$TXT_OUT"
        fi

        TRANSCRIPT=$(printf '%s' "$TRANSCRIPT" | tr -d '\n' | sed 's/^[[:space:]]*//')
        if [[ -z "$TRANSCRIPT" ]]; then
          step 'No speech detected'
          exit 1
        fi
        printf '%s' "$TRANSCRIPT"
      '';
    };

  # transcribeExpr sets TRANSCRIPT from $TMPFILE (shell snippet).
  mkFlavor =
    flavorId:
    { transcribeExpr, runtimeInputs }:
    let
      transcribeWav = pkgs.writeShellApplication {
        name = "speak2text-transcribe-wav-${flavorId}";
        runtimeInputs = [ pkgs.coreutils ] ++ runtimeInputs;
        text = ''
          set -euo pipefail
          TMPFILE="''${1:?usage: speak2text-transcribe-wav FILE.wav}"
          step() { printf '[speak2text-transcribe] %s\n' "$(date +%H:%M:%S) $*" >&2; }
          TRANSCRIPT=""
          ${transcribeExpr}
          TRANSCRIPT=$(printf '%s' "$TRANSCRIPT" | tr -d '\n' | sed 's/^[[:space:]]*//')
          if [[ -z "$TRANSCRIPT" ]]; then
            step 'No speech detected'
            exit 1
          fi
          printf '%s' "$TRANSCRIPT"
        '';
      };

      speak2text = pkgs.writeShellApplication {
        name = "speak2text";
        runtimeInputs = [
          pkgs.sox
          pkgs.ffmpeg
          transcribeWav
        ]
        ++ runtimeInputs;
        text = ''
          TMPFILE=$(mktemp /tmp/speak2text-XXXXXX.wav)
          trap 'rm -f "$TMPFILE"' EXIT

          step() { printf '[%s] %s\n' "$(date +%H:%M:%S)" "$*" >&2; }

          step 'Press Enter to start recording...'
          read -r _

          sox -d -r 16000 -c 1 -b 16 "$TMPFILE" 2>/dev/null &
          RECORD_PID=$!
          sleep 0.5

          if ! kill -0 "$RECORD_PID" 2>/dev/null; then
            step 'sox unavailable, falling back to ffmpeg'
            ffmpeg -f avfoundation -i ":0" \
              -ar 16000 -ac 1 -acodec pcm_s16le \
              -y -loglevel error "$TMPFILE" &
            RECORD_PID=$!
          fi

          step 'Recording... press Enter to stop.'
          read -r _
          kill -INT "$RECORD_PID" 2>/dev/null
          wait "$RECORD_PID" 2>/dev/null || true

          if [[ ! -s "$TMPFILE" ]]; then
            step 'No audio captured'
            exit 1
          fi

          step "Recorded $(wc -c < "$TMPFILE") bytes — transcribing..."

          if ! TRANSCRIPT=$("${transcribeWav}/bin/speak2text-transcribe-wav-${flavorId}" "$TMPFILE"); then
            step 'Transcription failed'
            exit 1
          fi

          printf '%s' "$TRANSCRIPT" | pbcopy
          step "Copied to clipboard: $TRANSCRIPT"
        '';
      };
    in
    {
      inherit speak2text transcribeWav;
    };

  flavorDefs = {
    whispercpp = {
      runtimeInputs = [ pkgs.whisper-cpp ];
      transcribeExpr = ''
        if [[ -z "''${WHISPER_MODEL:-}" ]]; then
          step 'Set $WHISPER_MODEL to the path of a ggml model file (e.g. ggml-base.en.bin)'
          exit 1
        fi
        TRANSCRIPT=$(whisper-cpp -m "''${WHISPER_MODEL}" -f "$TMPFILE" -nt 2>/dev/null || true)
      '';
    };
    "parakeet-mlx" = {
      runtimeInputs = [ pkgs.parakeet-mlx ];
      transcribeExpr = ''
        REAL_TMPFILE=$(realpath "$TMPFILE")
        TXT_OUT="''${REAL_TMPFILE%.wav}.txt"
        ( cd "$(dirname "$REAL_TMPFILE")" && parakeet-mlx --output-format txt "$REAL_TMPFILE" ) \
          && PARAKEET_EXIT=0 || PARAKEET_EXIT=$?
        if [[ $PARAKEET_EXIT -ne 0 ]]; then
          step "parakeet-mlx exited with code $PARAKEET_EXIT"
        fi
        TRANSCRIPT=$(cat "$TXT_OUT" 2>/dev/null || true)
        rm -f "$TXT_OUT"
      '';
    };
    "cohere-transcribe" = {
      runtimeInputs = [ ];
      transcribeExpr = ''
        MODEL_DIR="''${COHERE_TRANSCRIBE_MODEL_DIR:-''${HOME}/.local/share/cohere-transcribe/models/cohere-transcribe-03-2026}"
        TRANSCRIPT=$(${pkgs.cohere-transcribe}/lib/transcribe --model-dir "''${MODEL_DIR}" "$TMPFILE" 2>/dev/null || true)
      '';
    };
  };

  whisperPair = mkFlavor "whispercpp" flavorDefs.whispercpp;
  parakeetPair = mkFlavor "parakeet-mlx" flavorDefs."parakeet-mlx";
  coherePair = mkFlavor "cohere-transcribe" flavorDefs."cohere-transcribe";

  flavorPairs = {
    whispercpp = whisperPair;
    "parakeet-mlx" = parakeetPair;
    "cohere-transcribe" = coherePair;
  };

  selectedPair = flavorPairs.${cfg.flavor};

  # When parakeetServer is enabled and flavor is parakeet-mlx, use the
  # server-aware transcribe script instead of the CLI-only one.
  activeTranscribeWav =
    if cfg.parakeetServer && cfg.flavor == "parakeet-mlx" then
      parakeetMlxServerTranscribe
    else
      selectedPair.transcribeWav;

  transcribeBinPath = "${activeTranscribeWav}/bin/${activeTranscribeWav.name}";

  packages = {
    whispercpp = [
      pkgs.whisper-cpp
      whisperPair.speak2text
    ];
    "parakeet-mlx" = [
      pkgs.parakeet-mlx
      pkgs.parakeet-transcribe
      parakeet-mlx-init
      parakeetPair.speak2text
    ]
    ++ lib.optional cfg.parakeetServer pkgs.parakeet-mlx-server;
    "cohere-transcribe" = [
      pkgs.cohere-transcribe
      cohere-transcribe-init
      coherePair.speak2text
    ];
  };

  pttListener =
    if !isDarwin then
      null
    else
      let
        pythonPtt = pkgs.python3.withPackages (ps: [ ps.pyobjc-framework-Quartz ]);
      in
      pkgs.writeShellApplication {
        name = "speak2text-ptt-listener";
        runtimeInputs = [
          pythonPtt
          pkgs.sox
          pkgs.ffmpeg
          activeTranscribeWav
        ];
        text = ''
          export SPEAK2TEXT_TRANSCRIBE_BIN="${transcribeBinPath}"
          export SPEAK2TEXT_CONSUMER_KEYCODE="''${SPEAK2TEXT_CONSUMER_KEYCODE:-${cfg.pttConsumerKeycode}}"
          ${lib.optionalString cfg.parakeetServer ''
            export SPEAK2TEXT_SERVER_MODE=1
            export SPEAK2TEXT_SERVER_PORT="${toString cfg.parakeetServerPort}"
          ''}
          exec ${pythonPtt}/bin/python3 ${./speak2text-ptt-listener.py}
        '';
      };
in
{
  options.modules.speak2text = {
    enable = lib.mkEnableOption "speak2text";
    flavor = lib.mkOption {
      type = lib.types.enum [
        "whispercpp"
        "parakeet-mlx"
        "cohere-transcribe"
      ];
      default = "parakeet-mlx";
      description = "Speech-to-text backend. whispercpp: cross-platform CPU; parakeet-mlx: Apple Silicon MLX; cohere-transcribe: Rust + MLX (aarch64-darwin only).";
    };
    parakeetServer = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        When true and flavor is "parakeet-mlx", start the parakeet-mlx-server HTTP daemon
        (launchd on macOS) and use it for transcription instead of the CLI. Falls back to CLI
        if the server is not available. Eliminates 2–5 s model cold-start per call.
      '';
    };
    parakeetServerPort = lib.mkOption {
      type = lib.types.port;
      default = 5092;
      description = "TCP port for parakeet-mlx-server (when parakeetServer is enabled).";
    };
    pttConsumerKeycode = lib.mkOption {
      type = lib.types.str;
      default = "157";
      description = ''
        Decimal or 0x-prefixed NX aux keycode for push-to-talk (MY_GLOBE in yuanw/qmk-keymap:
        consumer usage 0x029d on press; macOS usually reports keycode 157 / 0x9d). Override if
        `skhd -o` or `/tmp/speak2text-ptt.log` shows a different code.
      '';
    };

    transcribeBin = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      readOnly = true;
      visible = false;
      description = "Absolute path to speak2text-transcribe-wav-* (set when speak2text is enabled).";
    };
  };

  config = lib.mkMerge (
    [
      (lib.mkIf cfg.enable {
        modules.speak2text.transcribeBin = transcribeBinPath;
        home-manager.users.${config.my.username} = {
          home.packages = packages.${cfg.flavor};
        }
        // lib.optionalAttrs config.modules.editors.emacs.enable {
          programs.emacs.init.usePackage.whisper = {
            # whisper.el records via ffmpeg (see whisper--record-command)
            extraPackages = [ pkgs.ffmpeg ];
            config =
              if cfg.parakeetServer && cfg.flavor == "parakeet-mlx" then
                let
                  port = toString cfg.parakeetServerPort;
                in
                ''
                  ;; Use parakeet-mlx HTTP server for transcription (server mode)
                  (with-eval-after-load 'whisper
                    (setq whisper-server-mode 'remote
                          whisper-server-host "127.0.0.1"
                          whisper-server-port ${port})

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
                                   "response_format=json"
                                   (concat "language=" whisper-language)))))

                    (defun my-whisper--check-model-consistency () t)

                    (advice-add 'whisper--transcribe-via-local-server :override
                                #'my-whisper--transcribe-via-parakeet-server)
                    (advice-add 'whisper--check-model-consistency :override
                                #'my-whisper--check-model-consistency))
                ''
              else
                ''
                  ;; Route whisper.el transcription through the same backend as CLI speak2text
                  ;; (https://github.com/natrys/whisper.el — whisper-install-whispercpp nil + whisper-command).
                  ;; whispercpp flavor: Emacs must inherit WHISPER_MODEL (ggml path), same as CLI speak2text.
                  (with-eval-after-load 'whisper
                    (defun whisper-command (input-file)
                      `("${transcribeBinPath}" ,input-file)))
                '';
          };
        };
      })
    ]
    ++ lib.optional isDarwin {
      environment.systemPackages = lib.mkIf cfg.enable (
        [
          selectedPair.speak2text
          activeTranscribeWav
          pttListener
        ]
        ++ lib.optional cfg.parakeetServer pkgs.parakeet-mlx-server
      );

      launchd.user.agents = lib.mkIf cfg.enable (
        {
          speak2text-ptt-listener = {
            serviceConfig = {
              ProgramArguments = [ "${pttListener}/bin/speak2text-ptt-listener" ];
              RunAtLoad = true;
              KeepAlive = true;
              ProcessType = "Interactive";
              StandardOutPath = "/tmp/speak2text-ptt.log";
              StandardErrorPath = "/tmp/speak2text-ptt.log";
            };
          };
        }
        // lib.optionalAttrs (cfg.parakeetServer && cfg.flavor == "parakeet-mlx") {
          parakeet-mlx-server = {
            serviceConfig = {
              ProgramArguments = [
                "${pkgs.parakeet-mlx-server}/bin/parakeet-mlx-server"
              ];
              EnvironmentVariables = {
                PARAKEET_MLX_PORT = toString cfg.parakeetServerPort;
              };
              RunAtLoad = true;
              KeepAlive = true;
              ProcessType = "Interactive";
              StandardOutPath = "/tmp/parakeet-mlx-server.log";
              StandardErrorPath = "/tmp/parakeet-mlx-server.log";
            };
          };
        }
      );
    }
  );
}
