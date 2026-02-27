{ pkgs, ... }:

with pkgs;
[
  # alerter
  pinentry_mac
  #yt-dlp
  #bandcamp-dl
  ffmpeg
  duti
  # github-mcp-server is now in nixpkgs, removed from mcp-servers-nix overlay
  # The overlay returns an empty-file placeholder that breaks buildEnv
  # TODO: Re-enable once overlay is updated or use pkgs.github-mcp-server directly
  # github-mcp-server

  #mcp-server-fetch
  slackdump
  #mcp-server-filesystem
  kanata-with-cmd
  defaultbrowser
  #sbcl
  #claude-code-acp
  #ghostty-mac
  #calibre_mac
]
