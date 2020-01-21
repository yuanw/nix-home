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

    # powerlevel10k is not avaiable at nixos-19.09
    # https://nixos.org/nixos/packages.html?channel=nixos-19.09&query=powerlevel10k
    ".config/zsh/custom/themes/powerlevel10k".source = pkgs.fetchFromGitHub {
                             owner = "romkatv";
                             repo = "powerlevel10k";
                             rev = "f1da8c41acb896f14024b1b07de4f9293fd06377";
                             sha256 = "1x6r1zhxhf0jk0adp35qjsw520sbvrfqrisbg9qz0kh7k8xc6rzl";
                           };

  };

  programs = {
    home-manager = {
      enable = true;
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
      ];

      enableAutosuggestions = true;
      history = {
        size = 50000;
        save = 500000;
        path = "${dotDir}/history";
        ignoreDups = true;
        share = true;
      };

      initExtra = lib.mkBefore ''
        export PATH=$PATH:/usr/local/bin:/usr/local/sbin
        . ${home_directory}/.nix-profile/etc/profile.d/nix.sh
        [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
        eval "$(direnv hook zsh)"

        function prev() {
           PREV=$(fc -lrn | head -n 1)
           sh -c "pet new `printf %q "$PREV"`"
        }

     '';
   
      oh-my-zsh = {
        enable = true;
        plugins =["git" "history"];
        custom = "$HOME/.config/zsh/custom";
        theme = "powerlevel10k/powerlevel10k";
      };
    };

    git = {
      enable = true;
      userName = "Yuan Wang";
      userEmail = "me@yuanwang.ca";

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
        github.user           = "yuanwang-wf";
        credential.helper     = "osxkeychain";
        "url \"git@github.com:\"".insteadOf = "https://github.com/";
      };
    };
 };
}
