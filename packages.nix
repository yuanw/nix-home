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
  wget
  unzip
  graphviz
  plantuml
  #zulu11.2.3-jdk11.0.1
  haskellPackages.niv
  nixpkgs-fmt

  #hledger
  #hledger-web
  #hledger-ui

  #OnePassword-op
  pass
  pass-git-helper

  HandBrake


  #knative
  kubectl
  minikube
  Docker

  rustc
  cargo


  google-cloud-sdk

  # productivity
  pet
  fd
  fzf
  ripgrep
  autojump
  silver-searcher
  python3

  #emacs
  vscode
  emacsGit
]
