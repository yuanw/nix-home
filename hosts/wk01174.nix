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
    (inputs.nix-home-private + "/modules/work.nix")
  ];
  users.users.${config.my.username}.uid = 505;
  my = {
    username = "yuanwang";
    name = "Yuan Wang";
    email = "yuan.wang@workiva.com";
    hostname = "WK01174";
    gpgKey = "19AD3F6B1A5BF3BF";
    workspaceDirectory = "workspaces";
    homeDirectory = "/Users/yuanwang";
  };

  # nix-gc / nix-store --optimise freeze this machine under load; run manually if needed
  launchd.daemons.nix-gc.serviceConfig.Disabled = true;
  launchd.daemons.nix-store-optimise.serviceConfig.Disabled = true;
  launchd.user.agents.user-nix-gc.serviceConfig.Disabled = true;
  #curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- repair sequoia --move-existing-users
  ids.uids.nixbld = 350;
  ids.gids.nixbld = 30000;
  environment.systemPath = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
  ];
  home-manager.users.${config.my.username} = {
    programs.git = {
      settings = {
        github.user = "yuanwang-wf";
        # url."git@github.com:".insteadOf = "https://github.com";
      };
    };
    home.packages = [
      (pkgs.writeShellScriptBin "pi-review" (builtins.readFile ../scripts/pi-review))
    ];
  };
  environment.casks = with inputs'.nix-casks.packages; [
    betterdisplay
    ungoogled-chromium
    slack
  ];
  modules = {
    # common = {
    #   enable = true;
    #   supportLocalVirtualBuilder = true;
    # };
    cursor.enable = true;
    herdr.enable = true;
    hunk.enable = true;
    open-code-review = {
      enable = true;
      settings = {
        provider = "dgx-spark";
        custom_providers.dgx-spark = {
          url = "http://dgx-spark.local:8888/v1";
          protocol = "openai";
          model = "Qwen3.8-Flash-Next";
          api_key = "not-needed";
          timeout_sec = 600;
        };
      };
    };
    speak2text = {
      enable = false;
      flavor = "parakeet-mlx";
      parakeetServer = true; # ← enables the server
      parakeetServerPort = 5092; # ← default, optional
    };
    transcribe.enable = true; # → transcribe, yt-dlp-librewolf
    pi = {
      enable = true;
      settings = {
        defaultProvider = "cursor-agent";
        defaultModel = "default";
      };
      extensionsPkgs = with pkgs.pi-extensions; [
        pi-review
        pi-cursor-agent
        pi-slow-mode
        pi-permission-gate
        pi-interactive-shell
        pi-ponytail
      ];
      extensionFiles = {
        "notify.ts" = ../modules/coding-agents/pi/extensions/notify.ts;
        "custom-footer.ts" = ../modules/coding-agents/pi/extensions/custom-footer.ts;
      };
      providers.dgx-spark = import ../modules/coding-agents/pi/providers/dgx-spark.nix {
        inherit inputs pkgs;
      };
      skills = [
        pkgs.pi-extensions.pi-interactive-shell
      ];
    };
    browsers.defaultBrowser = "librewolf";
    secrets.agenix = {
      enable = true;
    };
    neru.enable = true;
    brew = {
      enable = true;
      # taps = [ "homebrew/core" "homebrew/cask" ];
      casks = [
        "karabiner-elements"
        "viscosity"
      ];
      brews = [
        "redis"
        #"go"
      ];
    };
    browsers.librewolf.enable = true;
    editors.emacs = {
      enable = true;
      enableService = true;
      enableLatex = true;
      modalEditing = "hel";
    };
    # health.enable = true;
    dev = {
      # agda.enable = true;
      # ask.enable = true;
      dart.enable = true;
      java.enable = true;
      gcloud.enable = true;
      go.enable = true;
      playwright.enable = true;
      podman.enable = true;
      #scheme.enable = true;
      #haskell.enable = true;
      # lean.enable = true;
      idris2.enable = false;
      python.enable = true;
      zig.enable = false;
      racket.enable = false;
      kotlin.enable = true;
    };
    tmux = {
      enable = true;
      mainWorkspaceDir = "$HOME/workspaces";
      whichKey.enable = true;
    };
    terminal = {
      enable = true;
    };
    wm = {
      yabai.enable = true;
      yabai.enableJankyborders = true;
    };

    work = {
      enable = true;
      datadogMcp.enable = true;
      includeTrio = true;
      atlassianMcp.enable = true;
      atlassianMcp.readOnly = false;
    };
  };
}
