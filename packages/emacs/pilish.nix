# nix-prefetch-github dnouri pilish --rev <rev>
#
# Pilish is the Emacs frontend for the Pi coding agent (formerly
# pi-coding-agent).  Session browsing reads ~/.pi/agent/sessions JSONL
# via pilish-jsonl / pilish-browse; live chat uses pi RPC.
{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  transient,
  magit-section,
  md-ts-mode,
  markdown-table-wrap,
  ...
}:

melpaBuild {
  pname = "pilish";
  version = "3.4.4-unstable-2026-11-07";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "pilish";
    rev = "309fd210007a9493bb01f2cf248ae47277df2e46";
    sha256 = "sha256-v0G5Mh8ui7WliLyPXT4FZ1H/k8UAbYV9d3QU8cRj/nY=";
  };

  packageRequires = [
    transient
    magit-section
    md-ts-mode
    markdown-table-wrap
  ];

  # Every top-level .el that pilish.el requires at compile time must be listed
  # here, or byte-compiling fails with "Cannot open load file".
  recipe = writeText "recipe" ''
    (pilish
     :repo "dnouri/pilish"
     :fetcher github
     :files ("pilish.el"
              "pilish-browse.el"
              "pilish-core.el"
              "pilish-evil.el"
              "pilish-grammars.el"
              "pilish-input.el"
              "pilish-jsonl.el"
              "pilish-menu.el"
              "pilish-render.el"
              "pilish-snapshot.el"
              "pilish-table.el"
              "pilish-ui.el"))
  '';

  meta = with lib; {
    description = "Emacs frontend for the Pi coding agent";
    homepage = "https://github.com/dnouri/pilish";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
