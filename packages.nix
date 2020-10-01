{ pkgs }:

with pkgs;
[
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
  pet
  fd
  fzf
  ripgrep
  autojump
  vscode
  
  shellcheck
  sqlite
  editorconfig-core-c
  languagetool
  zstd

  # Fonts
  #fontconfig
  #lato
  #source-code-pro
  #font-awesome
  #material-design-icons
  #weather-icons
  #nerdfonts
  #zlib
  emacsMacport
]
