{ config, lib, pkgs, ... }:
let
  #homeDir = builtins.getEnv ("HOME");
  sources = import ../nix/sources.nix;
in with pkgs.stdenv;
with lib; {

  imports = [ ./modules ];
  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true; # default shell on catalina
  # programs.bash.enable = false;
  time.timeZone = "America/Regina";

  users.users.yuanwang.shell = pkgs.zsh;
  #users.users.yuanwang.home = homeDir;

  nixpkgs = {
    overlays = let path = ../overlays;
    in with builtins;
    map (n: import (path + ("/" + n))) (filter (n:
      match ".*\\.nix" n != null
      || pathExists (path + ("/" + n + "/default.nix")))
      (attrNames (readDir path))) ++ [
        (import (builtins.fetchTarball {
          inherit (sources.emacs-overlay) url sha256;
        }))
      ];

    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = false;
    };

  };
  home-manager.users.yuanwang = {
    home.packages = (import ./packages.nix { inherit pkgs; });

    home.file = {
      ".ghci".text = ''
        :set prompt "λ> "
      '';
    };

    xsession = {
      enable = true;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };

    programs = {
      direnv = {
        enable = true;
        enableZshIntegration = true;
        enableNixDirenvIntegration = true;
      };

      git = {
        enable = true;
        userName = "Yuan Wang";

        aliases = {
          co = "checkout";
          w = "status -sb";
          l = "log --graph --pretty=format:'%Cred%h%Creset"
            + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
            + " --abbrev-commit --date=relative --show-notes=*";
        };

        extraConfig = {
          core = {
            editor = "emacsclient -a '' -c";
            pager =
              "${pkgs.gitAndTools.delta}/bin/delta --plus-color=\"#012800\" --minus-color=\"#340001\" --theme='ansi-dark'";
          };
          branch.autosetupmerge = true;
          credential.helper =
            "${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper";
          # "url \"git@github.com:\"".insteadOf = "https://github.com/";
        };

        ignores = [ ".direnv" ".DS_Store" ];
      };

      #gpg = { enable = true; };

      home-manager = { enable = true; };

      jq = { enable = true; };

      alacritty = {
        enable = true;
        settings = {
          font = {
            normal = { family = "PragmataPro"; };
            size = 20;
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

      tmux = {
        enable = true;
        terminal = "screen-256color";
      };

      zoxide = {
        enable = true;
        enableZshIntegration = true;
      };

      zsh = rec {
        enable = true;
        dotDir = ".config/zsh";
        plugins = [{
          name = "powerlevel10k-config";
          src = lib.cleanSource ../conf.d/p10k-config;
          file = "p10k.zsh";
        }];

        sessionVariables = {
          PLANTUML_JAR_PATH = "${pkgs.plantuml}/lib/plantuml.jar";
          ASPELL_CONF = "data-dir ${pkgs.aspell}";
          LANG = "en_US.UTF-8";
          GITSTATUS_LOG_LEVEL = "DEBUG";
        };

        #shellAliases = { alerter = "${pkgs.alerter}/alerter"; };

        enableAutosuggestions = true;
        history = {
          size = 50000;
          save = 500000;
          path = "${dotDir}/history";
          ignoreDups = true;
          share = true;
        };

        initExtraBeforeCompInit =
          "source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme";

        initExtra = lib.mkBefore ''
          export PATH=$PATH:/usr/local/bin:/usr/local/sbin
          export PATH=$PATH:$HOME/.local/bin

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
  };
}
