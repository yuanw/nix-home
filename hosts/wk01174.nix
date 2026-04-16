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
    ../modules/private/work.nix
  ];
  users.users.${config.my.username}.uid = 501;
  my = {
    username = "yuanwang";
    name = "Yuan Wang";
    email = "yuan.wang@workiva.com";
    hostname = "WK01174";
    gpgKey = "19AD3F6B1A5BF3BF";
    workspaceDirectory = "workspaces";
    homeDirectory = "/Users/yuanwang";
  };
  #curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- repair sequoia --move-existing-users
  ids.uids.nixbld = 350;
  ids.gids.nixbld = 30000;
  environment.systemPath = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
  ];
  home-manager.users.${config.my.username}.programs = {
    git = {
      settings = {
        github.user = "yuanwang-wf";
        # url."git@github.com:".insteadOf = "https://github.com";
      };
    };
  };
  environment.casks =
    with inputs'.nix-casks.packages;
    [
      mouseless_preview
      betterdisplay
    ]
    ++ [ pkgs.vibeproxy ];
  modules = {
    # common = {
    #   enable = true;
    #   supportLocalVirtualBuilder = true;
    # };
    ai.enableOllama = true;
    droid.enable = true;
    cursor.enable = true;
    speak2text.enable = true;
    pi = {
      enable = true;
      enableWorkMux = true;
      extensionsPkgs = with pkgs.pi-extensions; [
        pi-loop
        pi-review
        pi-cursor-agent
        pi-slow-mode
        earendil-pi-review
      ];
      extensionFiles = {
        "permission-gate.ts" = ../modules/coding-agents/pi/extensions/permission-gate.ts;
        "notify.ts" = ../modules/coding-agents/pi/extensions/notify.ts;
        "custom-footer.ts" = ../modules/coding-agents/pi/extensions/custom-footer.ts;
      };
    };
    secrets.agenix = {
      enable = true;
    };
    claude-code = {
      enable = true;
      enableClaudeMem = true;
    };
    mouseless.enable = true;
    brew = {
      enable = true;
      # taps = [ "homebrew/core" "homebrew/cask" ];
      casks = [
        "karabiner-elements"
        "slack"
        "sloth"
        "ungoogled-chromium"
        "viscosity"
        #"librewolf"
        "firefox"
        "wispr-flow"
      ];
      brews = [
        "redis"
        #"go"
      ];
    };
    browsers.firefox = {
      enable = true;
      pkg = null;
    };
    editors.emacs = {
      enable = true;
      enableService = true;
      enableLatex = true;

      #enableAider = true;
      # enableCopilot = true;
      #lspStyle = "lsp-bridge";
    };
    # health.enable = true;
    dev = {
      # agda.enable = true;
      # ask.enable = true;
      dart.enable = true;
      java.enable = true;
      go.enable = true;
      playwright.enable = true;
      podman.enable = true;
      #scheme.enable = true;
      #haskell.enable = true;
      lean.enable = true;
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
      opensessions = {
        enable = true;
        width = 34;
        sidebarPosition = "right";
        showWindowDetails = true;
      };
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
      includeTrio = true;
    };
  };
}
