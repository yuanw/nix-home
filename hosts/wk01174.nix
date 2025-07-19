{
  inputs,
  config,
  ...
}:
{

  imports = [
    inputs.self.nixosModules.common
    inputs.self.nixosModules.darwin
    ../modules/private/work.nix
  ];

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
    go = {
      enable = true;
      goPath = "go";
    };
    git = {
      extraConfig = {
        github.user = "yuanwang-wf";
        # url."git@github.com:".insteadOf = "https://github.com";
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
      # taps = [ "homebrew/core" "homebrew/cask" ];
      casks = [
        "docker-desktop"
        "eloston-chromium"
        "slack"
        "sloth"
        "viscosity"
      ];
      brews = [
        "redis"
        #"go"
        # Compose is a Docker plugin. For Docker to find the plugin, add "cliPluginsExtraDirs" to ~/.docker/config.json:
        # "cliPluginsExtraDirs": [
        #  "$HOMEBREW_PREFIX/lib/docker/cli-plugins"
        # ]
        # ln -sfn $HOMEBREW_PREFIX/opt/docker-compose/bin/docker-compose ~/.docker/cli-plugins/docker-compose
        #"docker-compose"
      ];
    };
    browsers.firefox = {
      enable = true;
      # pkg = pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";
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
    typing.enable = true;
    dev = {
      # agda.enable = true;
      # ask.enable = true;
      dart.enable = true;
      java.enable = true;
      scheme.enable = true;
      #haskell.enable = true;
      idris2.enable = false;
      python.enable = true;
      zig.enable = false;
      racket.enable = false;
      kotlin.enable = true;
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

    work = {
      enable = true;
      includeTrio = true;
    };
  };
}
