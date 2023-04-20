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
  sass
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
  # nix-du

  devenv
  eukleides
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


  SDL2
  glew
  pkg-config
  # reiryoku-firmware
  # reiryoku-flash
]
