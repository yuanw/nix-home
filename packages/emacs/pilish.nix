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
  version = "3.3.0-unstable-2026-10-04";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "pilish";
    rev = "b8ab7fcdb2f7e177133200acbcd9c85b03f1c052";
    sha256 = "sha256-fE/lxuFHuonVfIzWTjOXyQQUpG2cCirt6DKKMfhRfCk=";
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
