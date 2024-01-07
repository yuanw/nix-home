{ pkgs, config, inputs, ... }: {
  imports = [
    inputs.self.nixosModules.common
    inputs.self.nixosModules.darwin
  ];

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
    browsers.firefox = {
      enable = true;
      pkg = pkgs.firefox-bin;
    };
    editors.emacs = {
      enable = true;
      enableService = true;
      enableDoomConfig = true;
    };

    terminal.enable = true;
    wm.yabai.enable = true;
    brew = {
      enable = true;
      # taps = [
      #   "homebrew/core"
      #   "homebrew/cask"
      # ];
      casks = [ "brave-browser" ];
    };
  };
}
