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
  ranger

  nixpkgs-fmt

  #Apps
  #HandBrake
  #wifi-password
  Stretchly
  #ihp
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

  shellcheck
  fontconfig

  #minikube
  #kubectl
  (ripgrep.override { withPCRE2 = true; })
  gnutls # for TLS connectivity

  ## Optional dependencies
  fd # faster projectile indexing
  imagemagick # for image-dired
  zstd
  ## Module dependencies
  # :checkers spell
  (aspellWithDicts (ds: [ ds.en ds.en-computers ds.en-science ]))
  # :checkers grammar
  languagetool
  # :tools editorconfig
  editorconfig-core-c # per-project style config
  # :tools lookup & :lang org +roam
  sqlite
  (python37.withPackages (ps:
    with ps; [
      pip
      ipython
      black
      isort
      setuptools
      pylint
      #poetry
      pytest
      pyflakes
    ]))
  # :lang latex & :lang org (latex previews)
  #texlive.combined.scheme-medium
]
