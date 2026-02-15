{ pkgs }:

with pkgs;
[
  #cacert
  clang
  coreutils-prefixed
  moreutils

  #ffmpeg
  less
  gifsicle
  graphviz
  htop
  nix-prefetch-git
  cachix
  # sass
  tree
  # broot
  wget
  #unrar
  unzip
  graphviz
  plantuml-c4
  xclip
  pass
  zlib
  pigz
  shellcheck
  entr

  pass-git-helper
  gh
  git-crypt

  #hledger
  #ranger
  dust
  nixpkgs-fmt
  nix-tree
  nix-diff
  nix-update
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
  # csvkit
  # dmenu
  #gum

  # TODO move to a modules
  # inkscape-with-extensions
  # ghostscript
  # djvu2pdf
  dig
  just
  pkg-config
  protobuf
  sioyek

  nix-prefetch-github
  internetarchive
  # racket
  # open-interpreter
  # reiryoku-firmware
  # reiryoku-flash
]
