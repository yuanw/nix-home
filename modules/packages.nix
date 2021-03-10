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
  git-crypt
  #gitAndTools.gitstatus
  ranger

  nixpkgs-fmt
  nix-tree

  #Apps
  #HandBrake
  #wifi-password
  Stretchly
  #ihp
  #nixops
  #nixfmt
  #nox
  #niv

  mu
  offlineimap
  notmuch
  google-cloud-sdk

  # productivity
  pandoc
  #fzf

  autojump
  vscode

  shellcheck
  fontconfig

  #minikube
  #kubectl

]
