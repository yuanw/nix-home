# nix-prefetch-github emacs-taf etaf-ui --rev <rev>
{
  melpaBuild,
  fetchFromGitHub,
  lib,

  # Elisp dependencies
  ebox,
  etaf,
  ...
}:
melpaBuild {
  pname = "etaf-ui";
  version = "0.3.3-unstable-2026-11-06";
  src = fetchFromGitHub {
    owner = "emacs-taf";
    repo = "etaf-ui";
    rev = "09af2e0a0649f3672d2ed3e1a3aabbb9e9b8d73a";
    sha256 = "sha256-mp7pv1kDdny3llmXa+xhOeF3AZNNCflAGOthrn0iRv0=";
  };

  # Package-Requires: ((emacs "29.1") (etaf "0.2.2") (ebox "3.0.1"))
  packageRequires = [
    etaf
    ebox
  ];

  meta = {
    homepage = "https://github.com/emacs-taf/etaf-ui";
    description = "ETAF component catalog: buttons, forms, panels and window shells";
    license = lib.licenses.gpl3Plus;
    maintainers = [ ];
  };
}
