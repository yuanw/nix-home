{ pkgs }:

with pkgs; [
  #cacert
  clang
  coreutils-prefixed
  moreutils
  #gnutls

  (aspellWithDicts (ds: [ ds.en ]))
  imagemagick
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
  texlive.combined.scheme-medium
  tree
  broot
  wget
  unrar
  unzip
  graphviz
  plantuml
  xclip
  pass
  emojify

  shellcheck
  editorconfig-core-c
  gitAndTools.pre-commit
  gitAndTools.delta
  gitAndTools.pass-git-helper
  gitAndTools.gh

  nixpkgs-fmt

  #Apps
  #HandBrake
  #wifi-password
  Stretchly
  dart

  #nixops
  #nixfmt
  #nox
  #niv
  #google-cloud-sdk

  mu
  offlineimap
  notmuch

  # productivity
  pandoc
  fd
  fzf
  ripgrep
  autojump
  vscode

  shellcheck
  sqlite
  editorconfig-core-c
  fontconfig
  languagetool
  zstd
]
