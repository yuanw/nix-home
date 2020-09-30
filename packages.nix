{ pkgs }:

with pkgs;
[
  #cacert
  clang
  coreutils
  moreutils
  gnutls

  (aspellWithDicts (ds: [ ds.en ]))
  imagemagick
  ffmpeg
  less
  gifsicle
  graphviz
  htop
  nix-prefetch-git
  sass
  #stack
  texlive.combined.scheme-medium
  tree
  broot
  wget
  shellcheck
  unzip
  graphviz
  plantuml
  editorconfig-core-c
  
  gitAndTools.pre-commit
  gitAndTools.delta
  gitAndTools.pass-git-helper
  gitAndTools.gh


  nixpkgs-fmt
  docker-credential-helpers

  pass

  #Apps
  #HandBrake
  Stretchly
  CopyQ
  #wifi-password

  nixops
  nixfmt
  nox
  niv
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


  universal-ctags
  pandoc

  emacsMacport
]
