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
  };

  # Let Home Manager install and manage itself.
  home.packages = [
    pkgs.ispell
    pkgs.ffmpeg
    pkgs.gifsicle
    pkgs.htop
    pkgs.nix-prefetch-git
    pkgs.sass
    pkgs.silver-searcher
    pkgs.stack
    pkgs.texlive.combined.scheme-full
    pkgs.tree
    pkgs.unzip
    pkgs.lorri
  ];

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
      userEmail = "me@yuanwang.ca";

      aliases = {
        co         = "checkout";
        w          = "status -sb";
        l          = "log --graph --pretty=format:'%Cred%h%Creset"
                     + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
                     + " --abbrev-commit --date=relative --show-notes=*";
      };

      extraConfig = {
        branch.autosetupmerge = true;
        github.user           = "yuanwang-wf";
        credential.helper     = "osxkeychain";

        "url \"git@github.com:\"".insteadOf = "https://github.com/";
      };
    };
  };
}
