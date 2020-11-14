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
  #gitAndTools.gitstatus


  nixpkgs-fmt

  #Apps
  #HandBrake
  #wifi-password
  Stretchly

  #nixops
  #nixfmt
  #nox
  #niv
  #google-cloud-sdk

  mu
  offlineimap
  notmuch
  google-cloud-sdk

  # productivity
  pandoc
  #fzf

  autojump
  vscode
  haskell-language-server

  shellcheck
  fontconfig
]
