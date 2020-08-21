{ pkgs }:

with pkgs;
[
  cacert
  clang
  coreutils
  moreutils
  ispell
  imagemagick
  ffmpeg
  less
  gifsicle
  #ghostscript
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
  gitAndTools.delta

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
  Stretchly
  CopyQ
  #wifi-password

  #rustc
  #cargo

  nixops
  dart
  google-cloud-sdk

  mu
  offlineimap
  notmuch


  # productivity
  pet
  fd
  fzf
  ripgrep
  autojump
  silver-searcher
  gh
  docker-credential-pass
  universal-ctags
  pandoc

  # Fonts
  fontconfig
  lato
  source-code-pro

  emacsUnstable
]
