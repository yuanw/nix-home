{
  melpaBuild,
  fetchFromGitHub,
  writeText,

  # Elisp dependencies

  websocket ? null,

  # Native dependencies
  ...
}:

let
  pname = "spookfox";
  version = "0.8.1";
in
melpaBuild {
  inherit pname version;
  src = fetchFromGitHub {
    owner = "bitspook";
    repo = "spookfox";
    rev = "26819f3d6faf5c6b8419ea68098adcceee42b1c4";
    sha256 = "sha256-ZsF6ZnEFNOarH6z3awvkbERhxvf5GpD1+nz9rElT7oc=";
  };

  # Only the Elisp side lives in lisp/; spookfox-addon/ is the browser add-on
  # (packaged separately in ../spookfox-firefox-extension.nix).
  files = ''("lisp/*.el")'';

  recipe = writeText "recipe" ''
    (spookfox
    :repo "bitspook/spookfox"
    :files ("lisp/*.el")
    :fetcher github)
  '';

  # The installed lisp/*.el are 0.8.1 (`sfjsi-' names) but the byte-compiled
  # .elc that comes out of the build are from an older spookfox
  # (`spookfox-js-injection--' names, Emacs 31.1), and Emacs loads the .elc.
  # Drop them so the .el that is actually shipped gets compiled on first load.
  preFixup = ''
    find "$out" -name '*.elc' -delete
  '';

  packageRequires = [ websocket ];
}
