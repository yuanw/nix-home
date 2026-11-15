# nix-prefetch-github emacs-taf ecss --rev <rev>
#
# Pure-Elisp CSS cascade engine; the base of the ETAF stack (ebox -> etaf ->
# etaf-ui -> etaf-pi).  All the code lives in `lisp/', which package-build's
# default files spec ("*.el" "lisp/*.el" ...) already covers, so no `files'.
{
  melpaBuild,
  fetchFromGitHub,
  lib,
  ...
}:
melpaBuild {
  pname = "ecss";
  version = "0.1.0-unstable-2026-10-22";
  src = fetchFromGitHub {
    owner = "emacs-taf";
    repo = "ecss";
    rev = "bfb553ae87c2cb2d385d825592c758cf77d39f44";
    sha256 = "sha256-/9BnCoQewvUTQCFd8zhUQ+CCZrCz4Citzfx1HQqjRM0=";
  };

  meta = {
    homepage = "https://github.com/emacs-taf/ecss";
    description = "Extensible CSS cascade engine: selectors, specificity and computed styles";
    license = lib.licenses.gpl3Plus;
    maintainers = [ ];
  };
}
