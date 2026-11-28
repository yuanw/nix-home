{ pkgs, ... }:

with pkgs;
[
  pinentry_mac
  bandcamp-dl
  ffmpeg
  duti
  slackdump
  defaultbrowser
  install-fonts
  # macOS has no `timeout`, and coreutils-prefixed (modules/packages.nix) only
  # names it gtimeout. Unprefixed coreutils would shadow BSD ls/date/du.
  (writeShellScriptBin "timeout" ''exec ${coreutils}/bin/timeout "$@"'')
  #sbcl
  #calibre_mac
]
