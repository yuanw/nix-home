{ pkgs, lib, config, ... }:

{
  home.username = config.my.username;
  home.homeDirectory = config.my.homeDirectory;
  home.stateVersion = "20.09";
  home.packages = (import ./modules/packages.nix { inherit pkgs; });
  home.file = {
    ".ghci".text = ''
      :set prompt "λ> "
    '';

  };
  programs = {

    alacritty = {
      enable = true;
      settings = {
        font = {
          normal = { family = "PragmataPro"; };
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
    # emacs = {
    #   enable = true;
    #   package = pkgs.emacsMacport;
    # };
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
      };
    };

    go = { enable = true; };
    gpg = { enable = true; };

    home-manager = { enable = true; };

    jq = { enable = true; };
    starship = {
      enable = true;
      enableZshIntegration = true;
      settings = {
        aws = { disabled = true; };
        gcloud = { disabled = true; };
        git_status = {
          ahead = "⇡($count)";
          diverged = "⇕⇡($ahead_count)⇣($behind_count)";
          behind = "⇣($count)";
          staged = "[++($count)](green)";
        };
      };
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

      # initExtraBeforeCompInit = ''
      #   source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
      #   source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/config/p10k-lean.zsh
      # '';

      initExtra = lib.mkBefore ''
        export PATH=$PATH:/usr/local/bin:/usr/local/sbin
        export PATH=$PATH:$HOME/.local/bin
        . /Users/yuanwang/.nix-profile/etc/profile.d/nix.sh

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
