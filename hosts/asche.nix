{ config, pkgs, ... }:

{
  my = {
    username = "yuanw";
    name = "Yuan Wang";
    email = "me@yuanwang.ca";
    hostname = "asche";
    gpgKey = "BF2ADAA2A98F45E7";
    homeDirectory = "/home/yuanw";
  };
  home-manager.users.${config.my.username} = {
    xdg.enable = true;
    home.file = {
      ".config/pass-git-helper/git-pass-mapping.ini".text = ''
        [github.com*]
        target=github
      '';
    };
    services.blueman-applet.enable = true;
    services.network-manager-applet.enable = true;
    services.dunst.enable = true;
    programs = {
      autorandr = {
        enable = true;
        profiles = {
          home = {
            fingerprint = {
              DP-3 =
                "00ffffffffffff001e6d0777aba10400091d0104b53c22789e3e31ae5047ac270c50542108007140818081c0a9c0d1c08100010101014dd000a0f0703e803020650c58542100001a286800a0f0703e800890650c58542100001a000000fd00383d1e8738000a202020202020000000fc004c472048445220344b0a20202001160203197144900403012309070783010000e305c000e3060501023a801871382d40582c450058542100001e565e00a0a0a029503020350058542100001a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000029";
              eDP-1 =
                "00ffffffffffff004d10cc14000000002d1d0104b51d12780e6e60a95249a1260d50540000000101010101010101010101010101010172e700a0f06045903020360020b41000001828b900a0f06045903020360020b410000018000000fe004b384a3057804c513133345231000000000002410332011200000b010a2020014202030f00e3058000e606050160602800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000aa";
            };
            config = {
              eDP-1 = {
                enable = true;
                crtc = 0;
                primary = true;
                position = "0x0";
                mode = "3840x2400";
                rate = "59.99";
              };
              DP-3 = {
                enable = true;
                crtc = 1;
                mode = "3840x2160";
                position = "3840x0";
                rate = "60.00";
              };
            };
          };
        };
      };
      password-store = { enable = true; };
      rofi = {
        enable = true;
        terminal = "${pkgs.alacritty}/bin/alaritty";
        theme = ../modules/theme.rafi;
      };
      git.extraConfig = {
        github.user = "yuanw";
        credential.helper =
          "${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper";
      };
    };
  };
  modules = {
    dev = { agda.enable = true;     haskell.enable = true;
    python.enable = false;
};
    terminal.enable = true;
    colemak.enable = true;
    wm.xmonad.enable = true;
    browsers.firefox.enable = true;
  };
  programs = {
    editors.emacs = {
      enable = true;
      enableDoomConfig = true;
      pkg = with pkgs;
        ((emacsPackagesFor emacsPgtkNativeComp).emacsWithPackages
          (epkgs: [ epkgs.vterm ]));
    };
  };
}
