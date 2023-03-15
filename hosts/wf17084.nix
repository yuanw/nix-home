{ lib, pkgs, config, services, ... }: {

  my = {

    username = "yuanwang";
    name = "Yuan Wang";
    email = "yuan.wang@workiva.com";
    hostname = "wf17084";
    gpgKey = "19AD3F6B1A5BF3BF";
    homeDirectory = "/Users/yuanwang";
  };
  home-manager.users.${config.my.username}.programs = {
    go = {
      enable = true;
      goPath = "go";
    };
    git = {
      extraConfig = {
        github.user = "yuanwang-wf";
        url."git@github.com:".insteadOf = "https://github.com";
      };
    };
  };
  modules = {
    brew = {
      enable = true;
      taps = [
        "homebrew/core"
        "homebrew/cask"
        "d12frosted/emacs-plus"
      ];

      casks = [
        "brave-browser"
        "docker"
        "firefox"
        "google-chrome"
        # "hammerspoon"
        "insomnia"
        # "karabiner-elements"
        "stretchly"
        "sloth"
      ];
      brews = [
        # "aws-iam-authenticator"
        # "helm"
       "emacs-plus@29"
        "pyenv"
        "pngpaste"
        "avr-gcc"
        "qmk/qmk/qmk"
        "jdtls"
        "redis"
      ];
    };
    browsers.firefox = {
      enable = true;
      pkg = pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";
    };
    editors.emacs = {
      enable = true;
      # enableService = true;
      pkg = pkgs.runCommand "emacs-0.0.0" { } "mkdir $out";
      # pkg = with pkgs;
      #   ((emacsPackagesFor emacsPlusNativeComp).emacsWithPackages
      #     (epkgs: [ epkgs.vterm ]));
    };

    typing.enable = true;
    dev = {
      agda.enable = false;
      python.enable = true;
      haskell.enable = true;
      dart.enable = true;
      # node.enable = true;
    };

    terminal = {
      enable = true;
      mainWorkspaceDir = "$HOME/workiva";
    };
    wm.yabai.enable = true;
  };
  programs = { workShell.enable = true; };
}
