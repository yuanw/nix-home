# nix-prefetch-github emacs-taf ebox --rev <rev>
#
# Box/layout layer of the ETAF stack.  `native/emoji/emoji-render.swift' is not
# built, so emoji rendering falls back to the Elisp path.
{
  melpaBuild,
  fetchFromGitHub,
  lib,

  # Elisp dependencies
  ecss,
  ...
}:
melpaBuild {
  pname = "ebox";
  version = "3.1.2-unstable-2026-11-14";
  src = fetchFromGitHub {
    owner = "emacs-taf";
    repo = "ebox";
    rev = "09f5ab30043a352dba1e15fa8022f95dcedbfc71";
    sha256 = "sha256-7qCHIiMmtNsYpTXMVlqvVUUyN9BZF/+Inob2IEC8ySQ=";
  };

  # Package-Requires: ((emacs "29.1") (ecss "0.1.0"))
  packageRequires = [ ecss ];

  meta = {
    homepage = "https://github.com/emacs-taf/ebox";
    description = "Box model, flex layout and viewport host for the ETAF stack";
    license = lib.licenses.gpl3Plus;
    maintainers = [ ];
  };
}
