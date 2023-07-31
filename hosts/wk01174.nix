{ pkgs, config, ... }: {

  config.my = {
    username = "yuanwang";
    name = "Yuan Wang";
    email = "yuan.wang@workiva.com";
    hostname = "WK01174";
    gpgKey = "19AD3F6B1A5BF3BF";
    homeDirectory = "/Users/yuanwang";
  };
  # TODO we need to add this back
  # environment.systemPath = [
  #   "/opt/homebrew/bin"
  #   "/opt/homebrew/sbin"
  # ];
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
        # url."git@github.com:".insteadOf = "https://github.com";
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
        "slack"
        "sloth"
        "mysql-shell"
        # "stretchly"
        "viscosity"
      ];
      brews = [
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
      enableService = true;
      enableDoomConfig = true;
      pkg = with pkgs;
        ((emacsPackagesFor pkgs.emacsPlusNativeComp).emacsWithPackages
          (epkgs: [ epkgs.vterm ]));
    };
    health.enable = true;
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
