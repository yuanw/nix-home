# nix-prefetch-github emacs-taf etaf --rev <rev>
#
# Text application framework facade.  `native/etaf-video-canvas.c' is not
# built, so the video component never gets the `canvas' image type.
{
  melpaBuild,
  fetchFromGitHub,
  lib,

  # Elisp dependencies
  ebox,
  ecss,
  ...
}:
melpaBuild {
  pname = "etaf";
  version = "0.3.5-unstable-2026-11-09";
  src = fetchFromGitHub {
    owner = "emacs-taf";
    repo = "etaf";
    rev = "59bf3c618a0a19e9b40894f390904146976c3291";
    sha256 = "sha256-SpGJ97s/3qFWtlRo8w49FQoEmOgX94KhA6EoBs9M3c4=";
  };

  # Package-Requires: ((emacs "29.1") (ebox "3.1.0") (ecss "0.1.0"))
  packageRequires = [
    ebox
    ecss
  ];

  meta = {
    homepage = "https://github.com/emacs-taf/etaf";
    description = "Text application framework facade: components, reactive state and hosts";
    license = lib.licenses.gpl3Plus;
    maintainers = [ ];
  };
}
