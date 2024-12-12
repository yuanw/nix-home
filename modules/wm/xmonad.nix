{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.wm.xmonad;
  xmonad-env = pkgs.haskellPackages.ghcWithHoogle (
    hp: with hp; [
      xmobar
      xmonad
      xmonad-contrib
      xmonad-extras
    ]
  );
  extra = ''
    ${pkgs.feh}/bin/feh --bg-fill --no-fehbg ~/.config/wallpapers/haskell-red-noise.png
  '';
in
{
  options.modules.wm.xmonad = {
    enable = mkEnableOption "xmonad";
  };

  config = mkIf cfg.enable {

    services.dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };

    networking.networkmanager = {
      enable = true;
    };
    programs.dconf.enable = true;
    services.upower.enable = true;
    services.xserver.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    services.xserver.libinput = {
      enable = true;
      touchpad.disableWhileTyping = true;
    };

    services.udisks2.enable = true;
    services.blueman.enable = true;
    services.xserver.displayManager = {
      defaultSession = "none+xmonad";
      lightdm.greeters.mini = {
        enable = false;
        user = "${config.my.username}";
        extraConfig = ''
          [greeter-theme]
          background-image = "";
          background-color = "#0C0F12"
          text-color = "#ff79c6"
          password-background-color = "#1E2029"
          window-color = "#181a23"
          border-color = "#bd93f9"
        '';
      };
    };
    # https://nixos.wiki/wiki/Extend_NixOS
    # https://www.freedesktop.org/software/systemd/man/systemd.service.html
    #systemd.user.services.flashfocus = {

    #  description = "flashfocus";
    #   after = [ "graphical-session-pre.target" ];

    #   wantedBy = [ "graphical-session.target" ];

    #   serviceConfig = {
    #     Type = "forking";
    #     User = "yuanw";
    #     ExecStart = ''${pkgs.flashfocus}/flashfocus'';
    #   };
    #  };

    home-manager.users.${config.my.username} = {
      home.packages = [
        xmonad-env
        (pkgs.writeShellScriptBin "autorandr-load-home" ''
          #
          # load autorandr home profile
          #
          xrandr --auto
          autorandr horizontal
          autorandr home
        '')
        #  pkgs.flashfocus
      ];

      # services.caffeine.enable = true;
      services.blueman-applet.enable = true;
      services.network-manager-applet.enable = true;
      services.dunst.enable = true;

      services.xscreensaver.enable = true;
      services.betterlockscreen.enable = true;
      services.pasystray.enable = true;
      services.udiskie = {
        enable = true;
        tray = "auto";
      };
      services.picom = {
        enable = true;
        settings = {
          detect-client-opacity = true;
        };
      };
      services.gnome-keyring.enable = true;
      services.trayer = {
        enable = true;
        settings = {
          edge = "top";
          align = "right";
          SetPartialStrut = true;
          monitor = "primary";
          expand = true;
          width = 10;
          transparent = true;
          height = 36;
          SetDockType = true;
          tint = "0x5f5f5f";

        };
      };
      xresources.properties = {
        "Xft.dpi" = 180;
        "Xft.autohint" = 0;
        "Xft.hintstyle" = "hintfull";
        "Xft.hinting" = 1;
        "Xft.antialias" = 1;
        "Xft.rgba" = "rgb";
        "Xcursor*theme" = "Vanilla-DMZ-AA";
        "Xcursor*size" = 24;
      };

      gtk = {
        enable = true;
        theme = {
          name = "dracula-theme";
          package = pkgs.dracula-theme;
        };
      };
      xsession = {
        enable = true;
        initExtra = extra;
        windowManager.xmonad = {
          enable = true;
          enableContribAndExtras = true;
          config = ../../packages/xmonad/xmonad.hs;

        };
      };
      programs.eww = {
        enable = true;
        configDir = ./eww;
      };
      programs.xmobar = {
        enable = true;
        extraConfig = ''
          Config { overrideRedirect = False
                 , font     = "xft:iosevka-12"
                 , bgColor  = "#5f5f5f"
                 , fgColor  = "#f8f8f2"
                 , position = TopSize L 90 36
                 , commands = [
                               Run Cpu
                                  [ "-L", "3"
                                  , "-H", "50"
                                  , "--high"  , "red"
                                  , "--normal", "green"
                                  ] 10
                              , Run Alsa "default" "Master"
                                  [ "--template", "<volumestatus>"
                                  , "--suffix"  , "True"
                                  , "--"
                                  , "--on", ""
                                  ]
                              , Run Memory ["--template", "Mem: <usedratio>%"] 10
                              , Run Swap [] 10
                              , Run DiskU [("/", "<fn=1>\xf0c7</fn>  hdd: <free> free")] [] 60
                              , Run Date "%a %Y-%m-%d <fc=#8be9fd>%H:%M</fc>" "date" 10
                              , Run XMonadLog
                              ]
                 , sepChar  = "%"
                 , alignSep = "}{"
                 , template = "%XMonadLog% }{ %alsa:default:Master% | %cpu% | %memory% * %swap% *  %disku% | %date% "
                 }
        '';
      };
    };
  };

}
