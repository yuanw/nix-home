{ pkgs, flake, ... }: {

   environment.systemPath = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
  ];
  home-manager.users.${flake.config.my.username}.programs = {
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
      };

    terminal = {
      enable = true;
      mainWorkspaceDir = "$HOME/workspaces";
    };
    wm.yabai.enable = true;
  };
  programs = { workShell.enable = true; };
}
