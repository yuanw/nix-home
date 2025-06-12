{ pkgs, ... }:

with pkgs;
[
  # alerter
  pinentry_mac
  yt-dlp
  bandcamp-dl
  ffmpeg
  duti
  github-mcp-server

  mcp-server-fetch

  mcp-server-slack
  mcp-server-filesystem
  #ghostty-mac
  #calibre_mac
]
