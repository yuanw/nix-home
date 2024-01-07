{ pkgs }:

with pkgs; [
  #cacert
  clang
  coreutils-prefixed
  moreutils

  ffmpeg
  less
  gifsicle
  graphviz
  htop
  nix-prefetch-git
  cachix
  nixfmt
  # sass
  tree
  # broot
  wget
  #unrar
  unzip
  graphviz
  plantuml
  xclip
  pass
  zlib
  pigz
  shellcheck
  gitAndTools.pre-commit
  gitAndTools.delta
  gitAndTools.pass-git-helper
  gitAndTools.gh
  git-crypt

  hledger
  ranger
  du-dust
  nixpkgs-fmt
  nix-tree
  nix-diff
  # nix-du
  lazygit
  gron
  keychain

  # devenv
  # eukleides
  #mu
  #offlineimap
  #notmuch
  google-cloud-sdk
  # qutebrowser
  # productivity
  pandoc

  # difftastic
  autojump
  # gap
  #shellcheck
  fontconfig
  #kubectl
  # vhs
  csvkit
  gum
  ghostscript
  djvu2pdf
  dig
  just
  pkg-config
  racket
  # open-interpreter
  # reiryoku-firmware
  # reiryoku-flash
]
