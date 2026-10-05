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

  # `transcribe` (below) drives yt-dlp and ffmpeg off PATH, and it picks its
  # speech-to-text backend by looking names up on that PATH: $TRANSCRIBE_ASR_CMD
  # if something sets it (nothing here does now), then $WHISPER_MODEL with
  # whisper.cpp, then `cohere-transcribe'.  writeShellApplication keeps the
  # caller's PATH behind the script's own, so what this list buys is `which
  # ffmpeg' on a machine you ssh into, and a `transcribe` started from somewhere
  # that never went through a login shell.
  #
  # /opt/homebrew/bin used to be here for brew's whisper.cpp, and
  # pkgs.cohere-transcribe with python3Packages.huggingface-hub beside it for
  # Cohere Transcribe and the tool that fetched its weights.  All three are gone:
  # nixpkgs carries whisper.cpp (as `whisper-cpp', installing `whisper-cli'), and
  # on the one long lecture the Cohere model spent 58 minutes echoing sentences
  # it had already said, which is not a transcript.
  environment.systemPath = [
    "${pkgs.transcribe}/bin"
    "${pkgs.whisper-cpp}/bin"
  ];
  home-manager.users.${config.my.username} = {
    programs.git.settings.github.user = "yuanw";
    programs.pi = {
      settings = {
        defaultProvider = "dgx-spark";
        defaultModel = "Qwen3.8-Flash-Next";
      };
      extensions = {
        notify.enable = true;
        custom-footer.enable = true;
        web-fetch.enable = true;
        cursor-agent.enable = true;
        slow-mode.enable = true;
        permission-gate.enable = true;
        interactive-shell.enable = true;
      };
    };
    home.packages = [
      (pkgs.writeShellScriptBin "pi-review" (builtins.readFile ../scripts/pi-review))
    ];
  };

  # Speech to text: `transcribe <URL|file>` (packages/transcribe.nix).  A video
  # with captions goes through yt-dlp; everything else goes through the model,
  # which is handed the media file and prints text on stdout.  A file with no
  # speech in it has nothing to transcribe and is filed as a failed file, which
  # is what the quality gate in packages/transcribe.nix is for.
  #
  # Nothing here starts by itself: whisper.cpp is a command, not a service.
  #
  # The weights are not in Nix and are not gated either -- plain downloads from
  # HuggingFace, so no HF_TOKEN is wanted for them.
  #   curl -L -o ~/.local/share/whisper/models/ggml-small.en.bin \
  #     https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-small.en.bin
  # (base.en is 142 MB, small.en 466 MB; whisper.cpp reads it with -m, and a
  # .bin outside the Nix store is not GC'd, so name it here once it is down).
  modules = {
    _1password.enable = true;
    transcribe = {
      enable = true;
      # asrCmd stays "", which leaves whisper.cpp as the backend on this
      # machine: found on PATH, and given this model.
      whisperModel = "${config.my.homeDirectory}/.local/share/whisper/models/ggml-small.en.bin";
    };
    # common = {
    #   enable = true;
    #   supportLocalVirtualBuilder = true;
    # };
    pi.enable = true;
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
