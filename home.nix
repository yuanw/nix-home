{ pkgs, lib, config, ... }:

{
  home.username = config.my.username;
  xdg.enable = true;
  home.homeDirectory = config.my.homeDirectory;
  home.stateVersion = "20.09";
  home.packages = (import ./modules/packages.nix { inherit pkgs; })
    ++ (import ./modules/linux_packages.nix { inherit pkgs; });

  home.file = {
    ".ghci".text = ''
      :set prompt "λ> "
    '';
    ".xmobarrc".text = ''
      Config { font = "xft:PragmataPro:size=18:bold"
              , borderColor = "black"
              , border = TopB
              , bgColor = "black"
              , fgColor = "grey"
              , allDesktops = True
              , position = TopW L 100
              , commands = [ Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                              -- Cpu usage in percent
                              , Run Cpu ["-t", "<fn=1>\xf108</fn>  cpu: (<total>%)","-H","50","--high","red"] 20
                              -- Ram used number and percent
                              , Run Memory ["-t", "<fn=1>\xf233</fn>  mem: <used>M (<usedratio>%)"] 20
                              -- Disk space free
                              , Run DiskU [("/", "<fn=1>\xf0c7</fn>  hdd: <free> free")] [] 60
                              , Run Swap [] 10
                              , Run Wireless "wlp0s20f3" [ ] 20
                              , Run DynNetwork [] 10
                              , Run Com "uname" ["-s","-r"] "" 36000
                              , Run Date "<fn=1>\xf133</fn> %a %b %_d %Y %H:%M:%S" "date" 10
                              , Run Battery ["-t", "<acstatus>: <left>% - <timeleft>",
      	                               "--",--"-c", "charge_full"
                                             "-O", "AC",
                                             "-o", "Bat",
                                             "-h", "green",
                                             "-l", "red"] 10
                              ]
              , sepChar = "%"
              , alignSep = "}{"
              , template = " %battery% | %cpu% | %memory% * %swap% | %disku% | %date% | %uname% | %wlp0s20f3wi% %dynnetwork% "
              }

    '';
    ".config/pass-git-helper/git-pass-mapping.ini".text = ''
      [github.com*]
      target=github
    '';

  };

  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad/xmonad.hs;
    };
  };

  programs = {
    alacritty = {
      enable = true;
      settings = {
        font = {
          normal = { family = "Roboto Mono"; };
          size = 18;
        };
        # https://github.com/dracula/alacritty/blob/master/dracula.yml
        colors = {
          primary.background = "0x282c34";
          primary.foreground = "0xabb2bf";

          cursor.text = "0x44475a";
          cursor.cursor = "0xf8f8f2";

          selection = {
            text = "0xf8f8f2";
            background = "0x44475a";
          };
          normal = {
            black = "0x000000";
            red = "0xff5555";
            green = "0x50fa7b";
            yellow = "0xf1fa8c";
            blue = "0xbd93f9";
            magenta = "0xff79c6";
            cyan = "0x8be9fd";
            white = "0xbfbfbf";
          };

          bright = {
            black = "0x4d4d4d";
            red = "0xff6e67";
            green = "0x5af78e";
            yellow = "0xf4f99d";
            blue = "0xcaa9fa";
            magenta = "0xff92d0";
            cyan = "0x9aedfe";
            white = "0xe6e6e6";
          };

          dim = {
            black = "0x14151b";
            red = "0xff2222";
            green = "0x1ef956";
            yellow = "0xebf85b";
            blue = "0x4d5b86";
            magenta = "0xff46b0";
            cyan = "0x59dffc";
            white = "0xe6e6d1";
          };

        };
      };
    };
    bat = { enable = true; };
    direnv = {
      enable = true;
      enableZshIntegration = true;
      enableNixDirenvIntegration = true;
    };

    git = {
      enable = true;
      userName = config.my.username;

      aliases = {
        co = "checkout";
        w = "status -sb";
        l = "log --graph --pretty=format:'%Cred%h%Creset"
          + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
          + " --abbrev-commit --date=relative --show-notes=*";
      };
      userEmail = config.my.email;

      signing = {
        key = config.my.gpgKey;
        signByDefault = true;
      };
      ignores = [ ".direnv" ".DS_Store" ];
      extraConfig = {
        core = {
          editor = "emacsclient -a '' -c";
          pager =
            "${pkgs.gitAndTools.delta}/bin/delta --plus-color=\"#012800\" --minus-color=\"#340001\" --theme='ansi-dark'";
        };
        branch.autosetupmerge = true;
        credential.helper =
          "${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper";
      };
    };

    gpg = { enable = true; };

    home-manager = { enable = true; };

    jq = { enable = true; };
    password-store = { enable = true; };
    rofi = {
      enable = true;
      terminal = "${pkgs.alacritty}/bin/alaritty";
      theme = ''
        /**
         * rofi -dump-theme output (based on arthur theme).
         * Rofi version: 1.5.4
         **/
        * {
            yellow:           rgba ( 232, 174, 91, 100 % );
            red:              rgba ( 205, 92, 92, 100 % );
            tcyan:            rgba ( 204, 176, 196, 87 % );
                    backlight:        rgba ( 204, 255, 238, 87 % );
                    lightgreen:       rgba ( 136, 204, 34, 100 % );
                    lightcyan:        rgba ( 176, 196, 222, 100 % );
                    green:            rgba ( 134, 175, 128, 100 % );
                    background-color: rgba ( 0, 0, 0, 0 % );
                    lightred:         rgba ( 204, 85, 51, 100 % );
                    font:             "JetBrainsMono Nerd Font 24";
                    tlightblack:      rgba ( 85, 68, 68, 80 % );
                    lightmagenta:     rgba ( 153, 102, 0, 100 % );
                    dark:             rgba ( 28, 28, 28, 100 % );
                    lightblack:       rgba ( 85, 68, 68, 100 % );
                    magenta:          rgba ( 222, 184, 135, 100 % );
                    black:            rgba ( 61, 53, 42, 100 % );
                    transparent:      rgba ( 0, 0, 0, 0 % );
                    lightwhite:       rgba ( 221, 204, 187, 100 % );
                    lightblue:        rgba ( 135, 206, 235, 100 % );
                    lightyellow:      rgba ( 255, 167, 93, 100 % );
                    cyan:             rgba ( 176, 196, 222, 100 % );
                    white:            rgba ( 187, 170, 153, 100 % );
                    foreground:       rgba ( 255, 238, 221, 100 % );
                    blue:             rgba ( 100, 149, 237, 100 % );
                    highlight:        bold underline rgba ( 255, 255, 255, 100 % );
                }
                window {
                    text-color:       var(magenta);
                    transparency:     "screenshot";
                    padding:          10px ;
                    children:         [ mainbox ];
                    orientation:      horizontal;
                    location:         center;
                    anchor:           center;
                    background-color: var(transparent);
                    border-radius:    10px ;
                    border:           0px ;
                    spacing:          0;
                }
                mainbox {
                    spacing:  0;
                    children: [ inputbar,message,listview ];
                }
                message {
                    padding:          5;
                    background-color: var(tcyan);
                    border-color:     var(foreground);
                    font:             "JetBrainsMono Nerd Font 20";
                    border:           0px 2px 2px ;
                    text-color:       var(black);
                }
                inputbar {
                    padding:          11px ;
                    background-color: var(tlightblack);
                    border:           2px ;
                    font:             "JetBrainsMono Nerd Font 30";
                    border-color:     var(foreground);
                    border-radius:    15px 15px 0px 0px ;
                    text-color:       var(lightgreen);
                }
                entry {
                    text-font:  inherit;
                    text-color: inherit;
                }
                prompt {
                    text-font:  inherit;
                    margin:     0px 0.3000em 0.0000em 0.0000em ;
                    text-color: inherit;
                }
                case-indicator {
                    text-font:  inherit;
                    text-color: inherit;
                }
                listview {
                    padding:          8px ;
                    background-color: rgba ( 28, 28, 28, 80 % );
                    dynamic:          false;
                    border-color:     var(foreground);
                    lines:            10;
                    border-radius:    0px 0px 15px 15px ;
                    border:           0px 2px 2px ;
                }
                element {
                    padding:          3px ;
                    background-color: rgba ( 0, 0, 0, 0 % );
                    vertical-align:   0.50;
                    font:             inherit;
                    border-radius:    4px ;
                    text-color:       var(foreground);
                }
                element selected.normal {
                    background-color: var(blue);
                }
                element selected.active {
                    foreground:       var(dark);
                    background-color: var(lightblue);
                }
                element selected.urgent {
                    foreground:       var(dark);
                    background-color: var(lightred);
                }
                element normal.active {
                    foreground: var(lightblue);
                }
                element normal.urgent {
                    foreground: var(lightred);
                }
                element alternate.active {
                    foreground: var(lightblue);
                }
                element alternate.urgent {
                    foreground: var(lightred);
                }
                vertb {
                    expand:   false;
                    children: [ dummy0,mode-switcher,dummy1 ];
                }
                dummy0 {
                    expand: true;
                }
                dummy1 {
                    expand: true;
                }
                mode-switcher {
                    orientation: vertical;
                    expand:      false;
                    spacing:     0px ;
                    border:      0px ;
                }
                button {
                    text-color:       var(foreground);
                    padding:          6px ;
                    background-color: var(tlightblack);
                    border-radius:    4px 0px 0px 4px ;
                    font:             "FontAwesome 22";
                    border:           2px 0px 2px 2px ;
                    horizontal-align: 0.50;
                    border-color:     var(foreground);
                }
                button selected.normal {
                    background-color: var(backlight);
                    border-color:     var(foreground);
                    text-color:       var(dark);
                    border:           2px 0px 2px 2px ;
                }
              '';
    };
    tmux = {
      enable = true;
      terminal = "screen-256color";
      clock24 = true;
      escapeTime = 1;
      keyMode = "vi";
      shortcut = "a";

      extraConfig = ''
        unbind -
        bind \| split-window -h
        bind - split-window
      '';
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };
    zsh = rec {
      enable = true;
      dotDir = ".config/zsh";

      sessionVariables = {
        PLANTUML_JAR_PATH = "${pkgs.plantuml}/lib/plantuml.jar";
        ASPELL_CONF = "data-dir ${pkgs.aspell}";
        LANG = "en_US.UTF-8";
        GITSTATUS_LOG_LEVEL = "DEBUG";
        EDITOR = "emacs";
      };

      enableAutosuggestions = true;
      history = {
        size = 50000;
        save = 500000;
        path = "$HOME/.config/zsh/history";
        ignoreDups = true;
        share = true;
      };

      initExtraBeforeCompInit = ''
        source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
        source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/config/p10k-lean.zsh
      '';

      initExtra = lib.mkBefore ''

        function prev() {
            PREV=$(fc -lrn | head -n 1)
            sh -c "pet new `printf %q "$PREV"`"
        }
      '';

      oh-my-zsh = {
        enable = true;
        plugins = [ "git" "history" "autojump" "history-substring-search" ];
        custom = "$HOME/.config/zsh/custom";
      };
    };

  };
}
