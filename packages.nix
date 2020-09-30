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
  lorri
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
  gh

  #python
  #python37Packages.pylint

  nixpkgs-fmt
  docker-credential-helpers
  #yabai


  #OnePassword-op
  pass
  #pass-git-helper

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

  # Fonts
  #fontconfig
  #lato
  #source-code-pro
  #font-awesome

  emacsMacport
]
