{ pkgs }:

with pkgs; [
  #cacert
  clang
  coreutils-prefixed
  moreutils
  #gnutls

  (aspellWithDicts (ds: [ ds.en ]))
  ffmpeg
  less
  gifsicle
  graphviz
  htop
  nix-prefetch-git
  cachix
  nixfmt
  sass
  stack
  texlive.combined.scheme-full
  tree
  wget
  unrar
  unzip
  graphviz
  plantuml
  #haskell-language-server-wrapper
  #haskell-language-server
  #haskellPackages.cabal-fmt
  nixpkgs-fmt
  xclip
  pass
  emojify

  HandBrake
  wifi-password
  stretchly

  # productivity
  pandoc
  pet
  fd
  fzf
  ripgrep
  autojump

  python3
  gitAndTools.gh
  gitAndTools.pre-commit
  gitAndTools.pass-git-helper
  vscode
  emacsUnstable
  shellcheck
  sqlite
  editorconfig-core-c
  languagetool
  zstd

  # Fonts
  fontconfig
  lato
  source-code-pro
  font-awesome
  material-design-icons
  weather-icons
  nerdfonts

  zlib
]
