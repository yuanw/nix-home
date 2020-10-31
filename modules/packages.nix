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
  #stack
  tree
  broot
  wget
  unrar
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
  gitAndTools.gitstatus


  nixpkgs-fmt

  #Apps
  #HandBrake
  #wifi-password
  Stretchly

  mu
  offlineimap
  notmuch
  google-cloud-sdk

  # productivity
  pandoc

  autojump
  vscode

  shellcheck
  fontconfig
]
