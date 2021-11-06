{ lib, pkgs, config, localConfig, services, ... }: {
  home-manager.users.${localConfig.username}.programs = {
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
      casks = [ "firefox" ];
      brews = [ "aws-iam-authenticator" ];
    };
    browsers.firefox = {
      enable = true;
      pkg = pkgs.runCommand "firefox-0.0.0" { } "mkdir $out";
    };
    dev = { julia.enable = true; };
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
