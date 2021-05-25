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
  broot
  wget
  #unrar
  unzip
  graphviz
  plantuml
  xclip
  pass

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
  nix-du

  #mu
  #offlineimap
  #notmuch
  google-cloud-sdk
  # qutebrowser
  # productivity
  pandoc

  autojump
  gap
  #shellcheck
  fontconfig
  #zellij
  #Docker
  #minikube
  #kubectl
]
