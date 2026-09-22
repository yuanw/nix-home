# nix-prefetch-github roman herdr.el --rev <rev>
#
# herdr.el is an Emacs porcelain over herdr's public JSON API: the panels
# (`herdr-spaces', `herdr-agents') read the session tree from the running herdr
# server, and `herdr-term' mirrors a pane through ghostel.  Load `herdr-ui',
# never a per-library entry point; `herdr-review' and `herdr-sound' are opt-in
# and left to `modules/editor/emacs/features/herdr.nix'.
{
  trivialBuild,
  fetchFromGitHub,
  lib,
  # Elisp dependencies
  magit-section,
  ghostel,
  ...
}:

trivialBuild {
  pname = "herdr-el";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "roman";
    repo = "herdr.el";
    rev = "c2b1cb93426736d2ced688b0e1a18ff1355e8958";
    sha256 = "sha256-Yd7is2aLvaDL29s18bBV9cOn4dv63qZXzawuyXpxcVs=";
  };

  packageRequires = [
    magit-section
    ghostel
  ];

  meta = with lib; {
    description = "Emacs porcelain for the herdr terminal workspace manager";
    homepage = "https://github.com/roman/herdr.el";
    license = licenses.gpl3Plus;
    platforms = platforms.all;
  };
}
