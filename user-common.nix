{ pkgs, ... }:

let home_directory = builtins.getEnv "HOME";
    log_directory = "${home_directory}/Library/Logs";
    tmp_directory = "/tmp";
    ca-bundle_crt = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    lib = pkgs.stdenv.lib;

in rec {
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = false;
    };

    overlays =
      let path = ./overlays_; in with builtins;
            map (n: import (path + ("/" + n)))
              (filter (n: match ".*\\.nix" n != null ||
                          pathExists (path + ("/" + n + "/default.nix")))
                (attrNames (readDir path)));
  };

  home.packages = import ./packages.nix { pkgs = pkgs;};

  home.file = {
    ".ghci".text = ''
                         :set prompt "λ> "
                         '';

    ".p10k.zsh".source  = ./.p10k.zsh;

    ".config/zsh/custom/plugins/zsh-history-substring-search".source = pkgs.fetchFromGitHub {
      owner = "zsh-users";
      repo = "zsh-history-substring-search";
      rev = "0f80b8eb3368b46e5e573c1d91ae69eb095db3fb";
      sha256 = "0y8va5kc2ram38hbk2cibkk64ffrabfv1sh4xm7pjspsba9n5p1y";
    };

  };

  fonts.fontconfig.enable = true;

  programs = {
    home-manager = {
      enable = true;
    };

    alacritty = {
      enable = true;
      settings = {
        font = {
          size = 20.0;
        };
        shell = {
          program =  "${pkgs.zsh}/bin/zsh";
        };
      };
    };

    direnv = {
      enable = true;
    };

    jq = {
      enable = true;
    };

    gpg = {
      enable = true;
    };

    zsh = rec {
      enable = true;
      dotDir = ".config/zsh";
      plugins = [
        { name = "iterm2_shell_integration";
          src = pkgs.fetchurl {
            url = https://iterm2.com/shell_integration/zsh;
            sha256 = "1qm7khz19dhwgz4aln3yy5hnpdh6pc8nzxp66m1za7iifq9wrvil";
            # date = 2020-01-07T15:59:09-0800;
          };
        }

        # powerlevel10k is not avaiable at nixos-19.09
        # https://nixos.org/nixos/packages.html?channel=nixos-19.09&query=powerlevel10k
        {
          file = "powerlevel10k.zsh-theme";
          name = "powerlevel10k";
          src = pkgs.fetchFromGitHub {
            owner = "romkatv";
            repo = "powerlevel10k";
            rev = "f1da8c41acb896f14024b1b07de4f9293fd06377";
            sha256 = "1x6r1zhxhf0jk0adp35qjsw520sbvrfqrisbg9qz0kh7k8xc6rzl";
          };
        }
      ];

      enableAutosuggestions = true;
      history = {
        size = 50000;
        save = 500000;
        path = "${dotDir}/history";
        ignoreDups = true;
        share = true;
      };

      sessionVariables = {
        PLANTUML_JAR_PATH =  "${pkgs.plantuml}/lib/plantuml.jar";
        DART_SDK = "${pkgs.dart}/bin";
        LANG = "en_US.UTF-8";
      };

      shellAliases = {
        ddev = "pub run dart_dev";
        pubcleanlock = ''git ls-files pubspec.lock --error-unmatch &>/dev/null && echo "Not removing pubspec.lock - it is tracked" || (rm pubspec.lock && echo "Removed pubspec.lock")'';
        pubclean = ''rm -r .pub .dart_tool/pub && echo "Removed .pub/"; find . -name packages | xargs rm -rf && echo "Removed packages/"; rm .packages && echo "Removed .packages"; pubcleanlock'';
        repub= "pubclean; pub get";
      };

      initExtra = lib.mkBefore ''
        export MANPATH="/usr/local/man:$MANPATH"
        export GOPATH="$HOME/go-workspace"
        export PATH=$PATH:/usr/local/bin:/usr/local/sbin
        export PATH="$HOME/.local/bin:$HOME/.pub-cache/bin:$PATH:$GOPATH/bin"


        eval "$(pyenv init -)"
        export PYENV_ROOT="$HOME/.pyenv" # needed by pipenv

        . ${home_directory}/.nix-profile/etc/profile.d/nix.sh
        [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
        eval "$(direnv hook zsh)"

        function prev() {
           PREV=$(fc -lrn | head -n 1)
           sh -c "pet new `printf %q "$PREV"`"
        }

        function dartUpgrade() {
           pub cache repair
           pub global activate dart_language_server
           pub global activate webdev_proxy
           pub global activate webdev


        }


        function bigskyTest {
                 python manage.py test $1 --http-integration --traceback -v 2
        }

        #THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
        export SDKMAN_DIR="/Users/yuanwang/.sdkman"
        [[ -s "/Users/yuanwang/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/yuanwang/.sdkman/bin/sdkman-init.sh"

     '';

      oh-my-zsh = {
        enable = true;
        plugins =["git"
                  #"pyenv"
                  "history"
                  "autojump"
                  "history-substring-search"];
        custom = "$HOME/.config/zsh/custom";
      };
    };

    git = {
      userName = "Yuan Wang";

      aliases = {
        co         = "checkout";
        w          = "status -sb";
        l          = "log --graph --pretty=format:'%Cred%h%Creset"
                     + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
                     + " --abbrev-commit --date=relative --show-notes=*";
      };

      extraConfig = {
        core = {
          editor = "emacs -nw";
          pager = "${pkgs.less}/bin/less --tabs=4 -RFX";
        };
        branch.autosetupmerge = true;
        credential.helper     = "${pkgs.pass-git-helper}/bin/pass-git-helper";
        "url \"git@github.com:\"".insteadOf = "https://github.com/";
      };
    };
 };
}
