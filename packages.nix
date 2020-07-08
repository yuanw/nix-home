{ pkgs }:

with pkgs;
[
  #cacert
  clang
  coreutils
  moreutils
  gnutls

  (aspellWithDicts (ds: [ ds.en ]))
  ffmpeg
  less
  gifsicle
  graphviz
  htop
  lorri
  nix-prefetch-git
  sass
  stack
  texlive.combined.scheme-medium
  tree
  wget
  unzip
  graphviz
  plantuml
  #haskellPackages.niv
  #haskellPackages.hoogle
  nixpkgs-fmt
  xclip

  #hledger
  #hledger-web
  #hledger-ui

  #OnePassword-op
  pass

  HandBrake
  wifi-password
  stretchly


  #knative
  #kubectl
  #minikube
  #Docker
  #kubernetes-helm
  #istio

  #rustc
  #cargo


  #google-cloud-sdk

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
  gitAndTools.pass-git-helper
  vscode
  emacsUnstable
  sqlite
  editorconfig-core-c
  languagetool
  zstd
]
