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
    ".config/zsh/.zshrc".source = ./zsh_config;

    ".config/ohmyzsh_custom/themes/powerlevel10k".source = pkgs.fetchFromGitHub {
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

      oh-my-zsh = {
        enable = true;
        custom = ".config/ohmyzsh_custom";
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
        };
        branch.autosetupmerge = true;
        github.user           = "yuanwang-wf";
        credential.helper     = "osxkeychain";
        "url \"git@github.com:\"".insteadOf = "https://github.com/";
      };
    };
  };
}
