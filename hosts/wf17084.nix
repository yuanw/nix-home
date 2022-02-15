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
    git = { extraConfig = { github.user = "yuanwang-wf"; }; };
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
        "hammerspoon"
        "insomnia"
        "karabiner-elements"
        "stretchly"
      ];
      brews = [ "aws-iam-authenticator" "pyenv" ];
    };
    browsers.firefox = {
      enable = true;
      pkg = pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";
    };
    dev = {
      julia.enable = true;
      agda.enable = true;
    };
    terminal = {
      enable = true;
      mainWorkspaceDir = "$HOME/workiva";
    };
    wm.yabai.enable = true;
  };
  programs = {
    node.enable = true;
    editors.emacs = {
      enable = true;
      pkg = pkgs.emacs;
    };
    python.enable = true;
    haskell.enable = true;
    dart.enable = true;
    workShell.enable = true;
  };
}
