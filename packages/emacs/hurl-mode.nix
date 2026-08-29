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
  version = "0-unstable-2026-08-13";
in
melpaBuild {
  inherit pname version;
  src = fetchFromGitHub {
    owner = "JasZhe";
    repo = "hurl-mode";
    rev = "ed2abc2b95d519cafff1f4db011f18462a76040a";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-QqxoiNFhDP05Kk+VMDbPM30x9Abob09WWyvrYLpcT74=";
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
