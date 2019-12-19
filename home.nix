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
      let path = ./overlays_ ; in with builtins;
            map (n: import (path + ("/" + n)))
              (filter (n: match ".*\\.nix" n != null ||
                          pathExists (path + ("/" + n + "/default.nix")))
                (attrNames (readDir path)));
  };

  home.packages = import ./packages.nix { pkgs = pkgs;};

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

    git = {
      enable = true;
      userName = "Yuan Wang";
      userEmail = "yuan.wang@workiva.com";

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
