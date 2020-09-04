{ pkgs }:

with pkgs;
[
  cacert
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
  lorri
  nix-prefetch-git
  sass
  stack
  texlive.combined.scheme-full
  tree
  broot
  wget
  shellcheck
  unzip
  graphviz
  plantuml
  xquartz
  #editorconfig-core-c
  python38Packages.editorconfig
  
  gitAndTools.pre-commit
  gitAndTools.delta

  #python
  python37Packages.pylint

  nixpkgs-fmt



  #OnePassword-op
  pass
  pass-git-helper

  #Apps
  #HandBrake
  Stretchly
  CopyQ
  #wifi-password

  nixops
  nixfmt
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
  #silver-searcher
  gitAndTools.gh

  #docker-credential-pass
  universal-ctags
  pandoc

  # Fonts
  #fontconfig
  lato
  source-code-pro
  font-awesome

  emacsUnstable
]
