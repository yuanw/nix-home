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
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };
  modules = {
    brew = {
      enable = true;
      casks = [
        "brave-browser"
        "docker"
        "firefox"
        "google-chrome"
        "qutebrowser"
        "hammerspoon"
        "insomnia"
        "karabiner-elements"
        "stretchly"
      ];
      brews = [ "pyenv"  "pngpaste"];
    };
    browsers.firefox = {
      enable = true;
      pkg = pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";
    };
    colemak.enable = true;
    dev = {
      agda.enable = false;
      python.enable = true;
      haskell.enable = true;
      dart.enable = true;
      node.enable = true;
    };

    terminal = {
      enable = true;
      mainWorkspaceDir = "$HOME/workiva";
    };
    wm.yabai.enable = true;
  };
  programs = {
    editors.emacs = {
      enable = true;
      pkg = pkgs.emacs;
    };
    workShell.enable = true;
  };
}
