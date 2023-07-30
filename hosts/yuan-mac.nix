{ pkgs, config, ... }: {
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

  modules = {
    common = { enable = true; };

    browsers.firefox = {
      enable = true;
      pkg = pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";
    };
    editors.emacs = {
      enable = true;
      # usePackage = false;
      pkg = with pkgs;
        ((emacsPackagesFor emacsPlusNativeComp).emacsWithPackages
          (epkgs: [ epkgs.vterm ]));
      enableDoomConfig = true;
    };

    terminal.enable = true;
    wm.yabai.enable = true;
    brew = {
      enable = true;
      taps = [
        "homebrew/core"
        "homebrew/cask"
        # "d12frosted/emacs-plus"
      ];
      # brews = [ "emacs-plus@29" ];
      casks = [ "brave-browser" "firefox" ];
    };
  };
  # programs = {
  #   node.enable = true;
  #   python.enable = false;
  #   haskell.enable = false;
  # };
}
