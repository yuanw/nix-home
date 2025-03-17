{
  pkgs,
  inputs,
  config,
  ...
}:
{

  imports = [
    inputs.self.nixosModules.common
    inputs.self.nixosModules.darwin

  ];

  my = {
    username = "yuanw";
    name = "Yuan Wang";
    hostname = "mist";
    homeDirectory = "/Users/yuanw";
  };

  environment.systemPath = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
  ];
  home-manager.users.${config.my.username}.programs = {
    go = {
      enable = true;
      goPath = "go";
    };
    git = {
      extraConfig = {
        github.user = "yuanw";
      };
    };
  };
  modules = {
    # common = {
    #   enable = true;
    #   supportLocalVirtualBuilder = true;
    # };
    ai.enable = true;
    secrets.agenix = {
      enable = true;
    };
    brew = {
      enable = true;
      casks = [
        "1password"
        "betterdisplay"
        "racket"
        "protonvpn"
        "brave-browser"
      ];
      masApps = {
        "Fresh Eyes" = 6480411697;
      };
      # taps = [ "homebrew/core" "homebrew/cask" ];
    };
    browsers.firefox = {
      enable = true;
      pkg = pkgs.firefox-beta-bin;
    };
    editors.emacs = {
      enable = true;
      enableLatex = false;
      enableService = true;
      # enableAider = true;
      # enableCopilot = true;
      lspStyle = "lsp-bridge";
    };
    # health.enable = true;
    typing.enable = true;
    jellyfin.enable = true;
    dev = {
      agda.enable = true;
      ask.enable = true;
      scheme.enable = true;
      #racket.enable = false;
      haskell.enable = false;
      idris2.enable = true;
      python.enable = true;
      zig.enable = false;
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
