{
  pkgs,
  config,
  inputs,
  ...
}:
{
  imports = [
    inputs.self.myModules.common
    inputs.self.myModules.darwin

  ];

  users.users.${config.my.username}.uid = 501;
  home-manager.users.${config.my.username} = {
    programs.git.settings = {
      github.user = "yuanw";
    };
    home.packages = [ pkgs.qbittorrent ];
  };
  # determinate system
  nix.enable = false;
  my = {
    username = "yuanw";
    name = "Yuan Wang";
    email = "me@yuanwang.ca";
    hostname = "yuanw";
    gpgKey = "BF2ADAA2A98F45E7";
    homeDirectory = "/Users/yuanw";
  };

  modules = {
    pi.enable = true;

    browsers = {
      librewolf.enable = true;
      defaultBrowser = "librewolf";
    };
    editors.emacs = {
      enable = true;
      enableService = true;
      enableLatex = false;
      # Try the nix-emacs-home library (modules/editor/emacs/lib.nix).
      useNixEmacsHome = true;
    };

    terminal.enable = true;
    wm.yabai.enable = true;
    brew = {
      enable = true;
      # taps = [
      #   "homebrew/core"
      #   "homebrew/cask"
      # ];
      casks = [
        "1password"
      ];
    };
  };
}
