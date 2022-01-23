{ lib, pkgs, config, ... }: {
  home-manager.users.${config.my.username}.programs.git = {
    extraConfig = { github.user = "yuanw"; };
  };

  my = {

    username = "yuanw";
    name = "Yuan Wang";
    email = "me@yuanwang.ca";
    hostname = "yuanw";
    gpgKey = "BF2ADAA2A98F45E7";
    homeDirectory = "/Users/yuanw";
  };

  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  modules = {

    browsers.firefox = {
      enable = true;
      pkg = pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";
    };

    terminal.enable = true;
    wm.yabai.enable = true;
    brew = {
      enable = true;
      casks = [
        "firefox"
        "docker"
        "google-chrome"
        "insomnia"
        "karabiner-elements"
        "stretchly"
        "hammerspoon"
      ];
    };
  };
  programs = {
    node.enable = true;
    python.enable = false;
    haskell.enable = true;
    editors.emacs = {
      enable = true;
      pkg = pkgs.emacs;
    };
    stevenBlackHosts.enable = false;
  };
}
