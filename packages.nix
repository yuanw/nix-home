{pkgs}:

with pkgs;
[
  cacert
  clang
  coreutils
  
  ispell
  ffmpeg
  less
  gifsicle
  graphviz
  htop
  lorri
  nix-prefetch-git
  sass
  stack
  texlive.combined.scheme-full
  tree
  broot
  wget
  unzip
  graphviz
  plantuml
  xquartz
  gitAndTools.pre-commit

  hledger
  hledger-web
  hledger-ui

  OnePassword-op
  pass
  pass-git-helper

  #Apps
  #HandBrake
  stretchly
  CopyQ
  #Docker

  #knative
  #kubectl
  #minikube
  nixops
  dart
  google-cloud-sdk
  emacs

  # productivity
  pet
  fd
  fzf
  ripgrep
  autojump
  silver-searcher
  gh
  docker-credential-pass

  # Fonts
  fontconfig
  lato
  source-code-pro

  # rust
  rustc
  cargo
]
