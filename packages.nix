{ pkgs }:

with pkgs;
[
  cacert
  clang
  coreutils
  moreutils
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
  wget
  unzip
  graphviz
  plantuml
  haskellPackages.niv
  haskellPackages.hoogle
  nixpkgs-fmt

  #hledger
  #hledger-web
  #hledger-ui

  #OnePassword-op
  pass
  pass-git-helper

  HandBrake
  wifi-password


  knative
  kubectl
  minikube
  Docker
  kubernetes-helm
  istio

  rustc
  cargo


  google-cloud-sdk

  #x11
  #xquartz
  #xorg.xhost
  #xorg.xauth
  #ratpoison
  #prooftree

  # productivity
  pet
  fd
  fzf
  ripgrep
  autojump
  silver-searcher
  python3
  gitAndTools.pre-commit

  #emacs
  vscode
  #emacsGit
  emacs72
]
