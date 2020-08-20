{ pkgs, ... }:

let
  home_directory = builtins.getEnv "HOME";
  log_directory = "${home_directory}/Library/Logs";
  tmp_directory = "/tmp";
  ca-bundle_crt = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  lib = pkgs.stdenv.lib;
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};

in
rec {
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = false;
    };

    overlays =
      let
        path = ./overlays_;
      in
        with builtins;
        map (n: import (path + ("/" + n)))
          (
            filter (
              n: match ".*\\.nix" n != null || pathExists (path + ("/" + n + "/default.nix"))
            )
              (attrNames (readDir path))
          ) ++ [
          (
            import (
              builtins.fetchTarball {
                url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
              }
            )
          )
        ];
  };


  home.packages = import ./packages.nix { pkgs = pkgs; };

  home.file = {
    ".ghci".text = ''
      :set prompt "λ> "
    '';


    ".config/zsh/custom/plugins/iterm2/iterm2.plugin.zsh".source = pkgs.fetchurl {
      url = https://iterm2.com/shell_integration/zsh;
      sha256 = "1qm7khz19dhwgz4aln3yy5hnpdh6pc8nzxp66m1za7iifq9wrvil";
      # date = 2020-01-07T15:59:09-0800;
    };

  };

  fonts.fontconfig.enable = true;

  programs = {
    home-manager = {
      enable = true;
    };

    # https://hugoreeves.com/posts/2019/nix-home/
    alacritty = {
      enable = false;
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
      enableZshIntegration = true;
    };

    jq = {
      enable = true;
    };

    go = {
      enable = false;
    };

    gpg = {
      enable = true;
    };

    z-lua = {
      enable = true;
      enableZshIntegration = true;
    };

    zsh = rec {
      enable = true;
      dotDir = ".config/zsh";
      plugins = [
        {
          name = "powerlevel10k";
          # src = pkgs.zsh-powerlevel10k;
          src = pkgs.fetchFromGitHub {
            owner = "romkatv";
            repo = "powerlevel10k";
            rev = "d53355cd30acf8888bc1cf5caccea52f486c5584";
            sha256 = "03v8qlblgdazbm16gwr87blm5nxizza61f8w6hjyhgrx51ly9ln5";
            # "date": "2020-03-15T08:43:52+01:00"
          };
          #file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
          file = "powerlevel10k.zsh-theme";
        }
        {
          name = "powerlevel10k-config";
          src = lib.cleanSource ./p10k-config;
          file = "p10k.zsh";
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
        DART_SDK = "${pkgs.dart}/dart";
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
        export PATH="$HOME/.local/bin:$HOME/.pub-cache/bin:$PATH:$GOPATH/bin:$DART_SDK:$DART_SDK/bin"
        eval "$(pyenv init -)"
        export PYENV_ROOT="$HOME/.pyenv" # needed by pipenv

        . ${home_directory}/.nix-profile/etc/profile.d/nix.sh
        export NIX_PATH=$NIX_PATH:$HOME/.nix-defexpr/channels

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
        clone      = "clone --recursive";
      };

      extraConfig = {
        core = {
          editor = "${pkgs.emacs}/bin/emacsclient -a '' -c";
          pager  = "${pkgs.less}/bin/less --tabs=4 -RFX";
        };
        branch.autosetupmerge = true;
        credential.helper     = "${pkgs.pass-git-helper}/bin/pass-git-helper";
        "url \"git@github.com:\"".insteadOf = "https://github.com/";
      };
    };
  };
}
