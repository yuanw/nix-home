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
  version = "3.4.5-unstable-2026-11-15";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "pilish";
    rev = "16da4b4fb20c7027da383ac95cf666ec65ec85cc";
    sha256 = "sha256-v1K2TAYStNO9nHp2dGlop/4N5qf83yKVixsFV44ZJco=";
  };

  packageRequires = [
    transient
    magit-section
    md-ts-mode
    markdown-table-wrap
  ];

  # "*.el" rather than an explicit list: upstream keeps adding top-level files
  # (pilish-conversations.el in 3.4.4 broke the build when it went unlisted) and
  # nothing at the repo root is meant to stay out of the package.
  recipe = writeText "recipe" ''
    (pilish
     :repo "dnouri/pilish"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "Emacs frontend for the Pi coding agent";
    homepage = "https://github.com/dnouri/pilish";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
