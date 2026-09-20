{
  melpaBuild,
  fetchFromGitHub,
  writeText,

  # Elisp dependencies

  # Native dependencies
  ...
}:

let
  pname = "hurl-mode";
  version = "0-unstable-2026-08-23";
in
melpaBuild {
  inherit pname version;
  src = fetchFromGitHub {
    owner = "JasZhe";
    repo = "hurl-mode";
    rev = "054a9bbf39a93528019d2274139dfae36e29c3cc";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-aO2Z2atB/CN5icXm6csOHpH6/wywZV7G1U13bPeIhb8=";
  };

  files = ''("*.el")'';

  postPatch = ''
    substituteInPlace hurl-mode.el \
      --replace-fail ';;; hurl-mode.el --- Major mode for hurl' \
                     ';;; hurl-mode.el --- Major mode for hurl  -*- lexical-binding: t; -*-'
  '';

  recipe = writeText "recipe" ''
    (hurl-mode
    :repo "jaszhe/hurl-mode"
    :files ("*.el")
    :fetcher github)
  '';

  packageRequires = [

  ];
}
