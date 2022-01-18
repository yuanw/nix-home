{ lib, pkgs, config, localConfig, ... }: {
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
    python.enable = false;
    haskell.enable = true;
    editors.emacs = {
      enable = true;
      pkg = pkgs.emacs;
    };
    stevenBlackHosts.enable = false;
  };
}
