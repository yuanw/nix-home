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
    ../modules/private/hledger.nix
    # ../modules/private/jellyfin-darwin.nix
  ];

  environment.casks = with inputs'.nix-casks.packages; [
    betterdisplay
    inputs'.nix-casks.packages."1password"
    godot
    racket
    vlc
  ];
  # determinate system
  nix.enable = false;
  my = {
    username = "yuan";
    name = "Yuan Wang";
    hostname = "mist";
    workspaceDirectory = "workspaces";
    homeDirectory = "/Users/yuan";
  };

  environment.systemPath = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
  ];
  home-manager.users.${config.my.username} = {
    programs.git.settings.github.user = "yuanw";
  };

  launchd.user.agents.dgx-spark-vllm-tunnel.serviceConfig = {
    Label = "ca.yuanwang.dgx-spark-vllm-tunnel";
    ProgramArguments = [
      "/usr/bin/ssh"
      "-N"
      "-L"
      "18000:127.0.0.1:8000"
      "-o"
      "ExitOnForwardFailure=yes"
      "-o"
      "ServerAliveInterval=30"
      "-o"
      "ServerAliveCountMax=3"
      "yuanw@dgx-spark.local"
    ];
    KeepAlive = true;
    RunAtLoad = true;
    StandardOutPath = "${config.my.homeDirectory}/Library/Logs/dgx-spark-vllm-tunnel.log";
    StandardErrorPath = "${config.my.homeDirectory}/Library/Logs/dgx-spark-vllm-tunnel.err.log";
  };
  modules = {
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
            baseUrl = "http://dgx-spark.local:8000/v1";
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
                id = "qwen3.8-flash-next";
                input = [
                  "text"
                  "image"
                ];
                maxTokens = 32768;
                name = "Qwen3.8 Flash Next (DGX Spark)";
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
