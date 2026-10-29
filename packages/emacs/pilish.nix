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
  version = "3.4.0-unstable-2026-10-28";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "pilish";
    rev = "b71c7f1d23ad49efca3ed69d99c932a605267a14";
    sha256 = "sha256-8Leq1sZgi9048PWowzkcYd1hc8Pj8/aIuai5kcHZXcw=";
  };

  packageRequires = [
    transient
    magit-section
    md-ts-mode
    markdown-table-wrap
  ];

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
