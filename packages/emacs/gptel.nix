{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  compat ? null,
  transient ? null,
  ...
}:

melpaBuild {
  pname = "gptel";
  version = "0.9.9.6-unstable-2026-11-14";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "1ce9cd8aba6dd65b8f8b437523399a7a577d91d1";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-Bw6dZDQa2Mu8+/47MaOHKGxkOIzVQtaccemzwoA3DxQ=";
  };

  packageRequires = [
    compat
    transient
  ];

  recipe = writeText "recipe" ''
    (gptel
     :repo "karthink/gptel"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "A simple LLM client for Emacs";
    homepage = "https://github.com/karthink/gptel";
    license = licenses.gpl3Only;
    maintainers = [ ];
    platforms = platforms.all;
  };
}
