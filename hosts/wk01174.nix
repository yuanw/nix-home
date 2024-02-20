{ pkgs, inputs, config, ... }: {

  imports = [
    inputs.self.nixosModules.common
    inputs.self.nixosModules.darwin
  ];

  my = {
    username = "yuanwang";
    name = "Yuan Wang";
    email = "yuan.wang@workiva.com";
    hostname = "WK01174";
    gpgKey = "19AD3F6B1A5BF3BF";
    homeDirectory = "/Users/yuanwang";
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
    secrets.agenix = { enable = true; };
    brew = {
      enable = true;
      # taps = [ "homebrew/core" "homebrew/cask" ];

      casks = [
        "docker"
        "google-chrome"
        "slack"
        "sloth"
        "mysql-shell"
        "viscosity"
      ];
      brews = [
        "redis"
        "frugal"
        "picat"
        # "mit-scheme"
        # https://formulae.brew.sh/formula/docker-compose
        # mkdir -p ~/.docker/cli-plugins
        # ln -sfn $HOMEBREW_PREFIX/opt/docker-compose/bin/docker-compose ~/.docker/cli-plugins/docker-compose
        "docker-compose"
      ];
    };
    browsers.firefox = {
      enable = true;
      pkg = pkgs.firefox-bin;
    };
    editors.emacs = {
      enable = true;
      enableService = true;
      enableDoomConfig = true;
    };
    health.enable = true;
    editors.neovim.enable = true;
    typing.enable = true;
    dev =
      {
        agda.enable = true;
        dart.enable = true;
        java.enable = true;
        haskell.enable = true;
        idris2.enable = true;
        python.enable = true;
        zig.enable = true;
        scheme.enable = true;
      };
    zellij.enable = false;
    tmux = {
      enable = true;
      mainWorkspaceDir = "$HOME/workspaces";
    };
    terminal = {
      enable = true;
    };
    wm.yabai.enable = true;

    workShell.enable = true;
  };
}
