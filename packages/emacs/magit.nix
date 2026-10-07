# nix-prefetch-github magit magit --rev <rev>
#
# magit pinned to master so the hi-lock fix
# (https://github.com/magit/magit/commit/e17085841ee50212bcd74507cbf42b7071a2ffa3,
# "magit-mode: Do not try to update hi-lock if it isn't loaded") is included;
# nongnu ELPA still ships 4.7.1 and the buggy code is newer than that release.
# Drop this override once the fix lands in a packaged ELPA release.
{
  melpaBuild,
  fetchFromGitHub,
  lib,
  # Elisp dependencies
  compat,
  cond-let,
  llama,
  magit-section,
  seq,
  transient,
  with-editor,
  ...
}:

melpaBuild {
  pname = "magit";
  version = "4.7.1-unstable-2026-10-02";
  src = fetchFromGitHub {
    owner = "magit";
    repo = "magit";
    rev = "e17085841ee50212bcd74507cbf42b7071a2ffa3";
    hash = "sha256-HknCwE9i7VBZZ198ogBCQJX2yCRH9ZGUvj5xCDld988=";
  };
  # MELPA recipe for magit; `git-hooks` dropped (not in the tree at this rev).
  files = ''
    ("lisp/magit*.el"
     "lisp/git-*.el"
     "docs/magit.texi"
     "docs/magit-section.texi"
     "docs/AUTHORS.md"
     "LICENSE"
     ".dir-locals.el"
     ("githooks" "githooks/*")
     (:exclude "lisp/magit-section.el"))
  '';
  packageRequires = [
    compat
    cond-let
    llama
    magit-section
    seq
    transient
    with-editor
  ];
  preferLocalBuild = true;
  allowSubstitutes = false;

  meta = with lib; {
    description = "It's Magit! A Git porcelain inside Emacs";
    homepage = "https://github.com/magit/magit";
    license = licenses.gpl3Plus;
    platforms = platforms.all;
  };
}
