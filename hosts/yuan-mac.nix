{ lib, pkgs, config, ... }:
let
  localConfig = {
    username = "yuanwang";
    name = "Yuan Wang";
    email = "yuan.wang@workiva.com";
    hostname = "wf17084";
    gpgKey = "19AD3F6B1A5BF3BF";
    homeDirectory = "/Users/yuanwang";
  };
in {
  home-manager.users.${localConfig.username}.programs.git = {
    extraConfig = { github.user = "yuanw"; };
  };
  modules = {
    terminal.enable = true;
    wm.yabai.enable = true;
    brew = {
      enable = true;
      casks = [
        "firefox"
        #"zoom"
        # bluejeans
      ];
    };
  };
  programs = {
    node.enable = true;
    python.enable = true;
    haskell.enable = true;
    editors.emacs = {
      enable = true;
      pkg = pkgs.emacs;
    };
    stevenBlackHosts.enable = true;
  };
}
