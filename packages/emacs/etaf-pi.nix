# nix-prefetch-github emacs-taf etaf-pi --rev <rev>
#
# Native Pi client for ETAF: Pi owns the conversation, ETAF owns the view.
# shortcut: `etaf-pi' spawns `pi --mode rpc' from PATH (protocol 0.87.1), so Pi
# must be reachable from Emacs' own environment.  Wrap with makeWrapper if the
# Emacs service cannot see Pi.
{
  melpaBuild,
  fetchFromGitHub,
  lib,

  # Elisp dependencies
  etaf,
  etaf-ui,
  ...
}:
melpaBuild {
  pname = "etaf-pi";
  version = "0.1.1-unstable-2026-10-22";
  src = fetchFromGitHub {
    owner = "emacs-taf";
    repo = "etaf-pi";
    rev = "bbd3dfd5c29ef342a3df3db2e5f491ca6d93c0c9";
    sha256 = "sha256-84dMT45K17lhkl5nHx5So84TCDoMXKMkAdwqtudgzfE=";
  };

  # Package-Requires: ((emacs "29.1") (etaf "0.3.0") (etaf-ui "0.2.0"))
  packageRequires = [
    etaf
    etaf-ui
  ];

  meta = {
    homepage = "https://github.com/emacs-taf/etaf-pi";
    description = "Native Pi client for ETAF: streaming chat, tool status and usage in Emacs";
    license = lib.licenses.gpl3Plus;
    maintainers = [ ];
  };
}
