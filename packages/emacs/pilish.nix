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
  version = "3.5.0-unstable-2026-11-25";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "pilish";
    rev = "328b3a7c9bb73ef32f70ed43a602496b742a0dd4";
    sha256 = "sha256-S8+JE4zqt4Ej5CY55kFtXntqBN9rS3XDI3LMQ+lm0L8=";
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
