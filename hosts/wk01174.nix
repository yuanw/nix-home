{ lib, pkgs, config, services, ... }: {

  my = {
    username = "yuanwang";
    name = "Yuan Wang";
    email = "yuan.wang@workiva.com";
    hostname = "WK01174";
    gpgKey = "19AD3F6B1A5BF3BF";
    homeDirectory = "/Users/yuanwang";
  };
  environment.variables = {
    PATH = [
      "/opt/homebrew/bin"
      "/opt/homebrew/sbin"
    ];
  };
  home-manager.users.${config.my.username}.programs = {
    go = {
      enable = true;
      goPath = "go";
    };
    # zsh = {
    #   initExtra = lib.mkAfter ''
    #       export PATH=/opt/homebrew/bin:$PATH
    #       export PATH=/opt/homebrew/sbin:$PATH
    #   '';
    # };
    git = {
      extraConfig = {
        github.user = "yuanwang-wf";
        url."git@github.com:".insteadOf = "https://github.com";
      };
    };
  };
  modules = {
    common = {
      enable = true;
      supportLocalVirtualBuilder = true;
    };
    secrets.agenix = { enable = true; };
    brew = {
      enable = true;
      taps = [ "homebrew/core" "homebrew/cask" ];

      casks = [
        "brave-browser"
        "docker"
        "firefox"
        "google-chrome"
        "insomnia"
        "stretchly"
        "sloth"
        "slack"
      ];
      brews = [
        # "aws-iam-authenticator"
        # "helm"
        # "go"
        # "pyenv"
        # "pngpaste"
        "jdtls"
        "redis"
        "frugal"
      ];
    };
    browsers.firefox = {
      enable = true;
      pkg = pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";
    };
    editors.emacs = {
      enable = true;
      enableDoomConfig = true;
      pkg = with pkgs;
        ((emacsPackagesFor emacsPlusNativeComp).emacsWithPackages
          (epkgs: [ epkgs.vterm ]));
    };
    editors.neovim.enable = true;
    typing.enable = true;
    dev =
      {
        agda.enable = false;
        dart.enable = true;
        haskell.enable = true;
        python.enable = true;
        # zig.enable = true;
        # node.enable = true;
      };

    terminal = {
      enable = true;
      mainWorkspaceDir = "$HOME/workspaces";
    };
    wm.yabai.enable = true;
  };
  programs = { workShell.enable = true; };
}
