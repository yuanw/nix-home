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
  broot
  wget
  unzip
  graphviz
  plantuml
  xquartz
  gitAndTools.pre-commit

  #python
  python37Packages.pylint

#  haskellPackages.niv
#  haskellPackages.hoogle
#  nixpkgs-fmt

  #hledger
  #hledger-web
  #hledger-ui

  #OnePassword-op
  pass
  pass-git-helper

  #Apps
  #HandBrake
  stretchly
  CopyQ
  #wifi-password

  #rustc
  #cargo

  nixops
  dart
  google-cloud-sdk


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

  emacsUnstable
]
