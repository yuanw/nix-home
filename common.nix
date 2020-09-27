{ pkgs, ... }:
let
  home_directory = builtins.getEnv "HOME";
  log_directory = "${home_directory}/Library/Logs";
  tmp_directory = "/tmp";
  #ca-bundle_crt = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  lib = pkgs.stdenv.lib;
in rec {
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = false;
    };

    overlays = let path = ./overlays; in with builtins;
    map (n: import (path + ("/" + n))) (filter (n:
      match ".*\\.nix" n != null
      || pathExists (path + ("/" + n + "/default.nix")))
      (attrNames (readDir path))) ++ [
        (import (builtins.fetchTarball {
          url =
            "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
        }))
      ];
  };

  home.packages = import ./packages.nix { pkgs = pkgs; };

  home.file = {
    ".ghci".text = ''
      :set prompt "λ> "
    '';

    ".config/zsh/custom/plugins/iterm2/iterm2.plugin.zsh".source =
      pkgs.fetchurl {
        url = "https://iterm2.com/shell_integration/zsh";
        sha256 = "1gw3rk0dsss3vl92wxpda7br8gmwrx6jk41xm3i3rh6p2d7r97z0";
        # date = 2020-01-07T15:59:09-0800;
      };

  };

  fonts.fontconfig.enable = true;

  programs = {
    home-manager = { enable = true; };

    direnv = {
      enable = true;
      enableZshIntegration = true;
      enableNixDirenvIntegration = true;
    };

    jq = { enable = true; };

    gpg = { enable = true; };

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

      sessionVariables = {
        PLANTUML_JAR_PATH = "${pkgs.plantuml}/lib/plantuml.jar";
        ASPELL_CONF = "data-dir ${pkgs.aspell}";
        LANG = "en_US.UTF-8";
      };

      enableAutosuggestions = true;
      history = {
        size = 50000;
        save = 500000;
        path = "${dotDir}/history";
        ignoreDups = true;
        share = true;
      };

      initExtra = lib.mkBefore ''
        export PATH=$PATH:/usr/local/bin:/usr/local/sbin:$HOME/.emacs.d/bin:$HOME/.local/bin
        export NIX_PATH=$NIX_PATH:$HOME/.nix-defexpr/channels

        function prev() {
           PREV=$(fc -lrn | head -n 1)
           sh -c "pet new `printf %q "$PREV"`"
        }

      '';

      oh-my-zsh = {
        enable = true;
        plugins = [ "git" "history" ];
        custom = "$HOME/.config/zsh/custom";
      };
    };

    git = {
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
          editor = "${pkgs.emacsUnstable}/bin/emacsclient -a '' -c";
          pager =
            "${pkgs.gitAndTools.delta}/bin/delta --plus-color=\"#012800\" --minus-color=\"#340001\" --theme='ansi-dark'";
        };
        branch.autosetupmerge = true;
        credential.helper =
          "${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper";
        # "url \"git@github.com:\"".insteadOf = "https://github.com/";
      };
    };
  };
}
