{
  inputs,
  inputs',
  config,
  pkgs,
  ...
}:
{

  imports = [
    inputs.self.myModules.common
    inputs.self.myModules.darwin
    ../modules/hledger.nix
    # jellyfin-darwin.nix now lives in the nix-home-private repo (modules/jellyfin-darwin.nix)
  ];

  # 1Password comes from modules/_1password.nix now (pkgs._1password-gui into
  # /Applications, op into /usr/local/bin) - a cask copy here would register the
  # same bundle twice and fight the CLI integration.
  environment.casks = with inputs'.nix-casks.packages; [
    betterdisplay
    godot
    racket
    vlc
  ];
  # determinate system
  nix.enable = false;

  # Phone/remote access for Herdr: join this Mac to a tailnet, then SSH in and run `herdr`.
  # First login still needs: sudo tailscale up
  services.tailscale.enable = true;
  services.openssh.enable = true;
  my = {
    username = "yuan";
    name = "Yuan Wang";
    hostname = "mist";
    workspaceDirectory = "workspaces";
    homeDirectory = "/Users/yuan";
  };

  # `transcribe` (below) drives yt-dlp and ffmpeg off PATH.  It picks its
  # speech-to-text backend the same way: $TRANSCRIBE_ASR_CMD if set, then
  # $WHISPER_MODEL with whisper-cpp, then `cohere-transcribe' looked up on PATH
  # with $COHERE_TRANSCRIBE_MODEL_DIR as the weights directory.  Those four
  # arrive in that script's own closure either way -- writeShellApplication
  # keeps the caller's PATH behind it, and that has /run/current-system/sw/bin
  # in it -- so what this list buys is `which ffmpeg` on a machine you ssh into,
  # and a `transcribe` started from somewhere that never went through a login
  # shell.  huggingface-cli is how the gated weights get fetched.
  environment.systemPath = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
    "${pkgs.transcribe}/bin"
    "${pkgs.cohere-transcribe}/bin"
    "${pkgs.python3Packages.huggingface-hub}/bin"
  ];
  home-manager.users.${config.my.username} = {
    programs.git.settings.github.user = "yuanw";
  };

  # The weights directory, for the two ways `transcribe` can be told about a
  # speech-to-text backend: modules.transcribe.asrCmd below (a session variable,
  # so login shells only) and this, which any process reading the environment
  # gets.  `cohere-transcribe' takes the model directory as --model-dir and has
  # no default, which is why it is named in both.
  environment.variables.COHERE_TRANSCRIBE_MODEL_DIR = "${config.my.homeDirectory}/.local/share/cohere-transcribe/models/cohere-transcribe-03-2026";

  # Speech to text: `transcribe <URL|file>` (packages/transcribe.nix), with
  # Cohere Transcribe (pkgs.cohere-transcribe, the Rust CLI from
  # second-state/cohere_transcribe_rs) as the speech-to-text backend.  A video
  # that has captions goes through yt-dlp; everything else goes through the
  # model, which is handed the media file and prints text on stdout -- the shape
  # $TRANSCRIBE_ASR_CMD expects.  --model-dir has to be part of that command:
  # the CLI has no default model directory, and $HOME is not expanded inside an
  # environment variable.
  #
  # Nothing here starts by itself.  The model wants 7-8 GB while it runs, on a
  # machine with 16 GB of unified memory, so transcription is something to run
  # and not a daemon to keep resident.
  #
  # `transcribe -A` (--force-asr) skips the caption lookup, which is how to make
  # the model run on a video that does have captions: without it the captions
  # win, being the cheaper correct text.
  #
  # The weights are gated on HuggingFace (accept the licence, then HF_TOKEN) and
  # are deliberately not fetched by Nix.  One time, by hand:
  #   huggingface-cli download CohereLabs/cohere-transcribe-03-2026 \
  #     --local-dir ~/.local/share/cohere-transcribe/models/cohere-transcribe-03-2026
  # and copy vocab.json -- it ships inside the cohere-transcribe package, under
  # share/cohere-transcribe/ -- next to the weights: the model directory wants
  # config.json, model.safetensors and vocab.json together.  huggingface-cli
  # comes from python3Packages.huggingface-hub, which joins cohere-transcribe in
  # environment.systemPath above.  With no model directory `transcribe` still
  # does captioned videos and says so for the rest; whisper.cpp (1 to 2 GB, no
  # licence to accept) is the way out if the model needs too much memory.
  modules = {
    _1password.enable = true;
    transcribe = {
      enable = true;
      asrCmd = "${pkgs.cohere-transcribe}/bin/cohere-transcribe --model-dir ${config.my.homeDirectory}/.local/share/cohere-transcribe/models/cohere-transcribe-03-2026";
    };
    # common = {
    #   enable = true;
    #   supportLocalVirtualBuilder = true;
    # };
    pi = {
      enable = true;
      extensionsPkgs = with pkgs.pi-extensions; [
        pi-loop
        pi-review
        pi-cursor-agent
        pi-slow-mode
        pi-permission-gate
        pi-mcp-adapter
        pi-interactive-shell
      ];
      extensionFiles = {
        "notify.ts" = ../modules/coding-agents/pi/extensions/notify.ts;
        "custom-footer.ts" = ../modules/coding-agents/pi/extensions/custom-footer.ts;
        "web-fetch.ts" = ../modules/coding-agents/pi/extensions/web-fetch.ts;
      };
      models = {
        providers = {
          dgx-spark = {
            api = "openai-completions";
            apiKey = "not-needed";
            baseUrl = "http://dgx-spark.local:8888/v1";
            compat = {
              supportsDeveloperRole = false;
              supportsReasoningEffort = false;
              supportsStore = false;
              thinkingFormat = "qwen-chat-template";
              thinkingTokenBudgetField = "thinking_token_budget";
            };
            models = [
              {
                _launch = true;
                contextWindow = 262144;
                # Must match the served name exactly (TensorFold SERVED_NAME).
                id = "Qwen3.8-Flash-Next";
                input = [
                  "text"
                  "image"
                ];
                maxTokens = 32768;
                name = "Qwen3.8 Flash Next (DGX Spark, TensorFold)";
                reasoning = true;
                thinkingLevelMap = {
                  off = "off";
                  minimal = "minimal";
                  low = "low";
                  medium = "medium";
                  high = "high";
                  xhigh = "xhigh";
                  max = "max";
                };
              }
            ];
          };
        };
      };

    };
    secrets.agenix = {
      enable = true;
    };
    brew = {
      enable = true;
      masApps = {
        "Fresh Eyes" = 6480411697;
        "Keystroke Pro" = 1572206224;
      };
      # taps = [ "homebrew/core" "homebrew/cask" ];
    };
    neru.enable = true;
    herdr.enable = true;
    browsers = {
      librewolf.enable = true;
      defaultBrowser = "librewolf";
    };
    editors.emacs = {
      enable = true;
      enableLatex = false;
      enableService = true;

      modalEditing = "hel";

    };
    # health.enable = true;
    #jellyfin.enable = true;
    dev = {
      #agda.enable = true;
      #ask.enable = true;
      scheme.enable = true;
      # lean.enable = true;
      #racket.enable = false;
      haskell.enable = false;
      #idris2.enable = true;
      python.enable = true;
      #zig.enable = false;
    };

    hermes-agent = {
      enable = false;
      enableGateway = false;
      enableDashboard = false;
      environment = {
        DEEPSEEK_BASE_URL = "http://dgx-spark.local:8000/v1";
        DEEPSEEK_API_KEY = "not-needed";
      };
      config = {
        model = "deepseek-v4-flash";
        custom_providers = [
          {
            name = "dgx-spark";
            base_url = "http://dgx-spark.local:8000/v1";
          }
        ];
      };
    };
    tmux = {
      enable = true;
      mainWorkspaceDir = "$HOME/workspaces";
    };
    terminal = {
      enable = true;
    };
    wm = {
      yabai.enable = true;
      yabai.enableJankyborders = true;
    };
  };
}
