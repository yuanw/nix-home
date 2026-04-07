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
  version = "0.9.9.4-unstable-2026-04-07";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "cdbcdcbcef2152be97420dd737da08c0c51a324b";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-rCQjpU+eu/SIOiQWB5NFhYqyu9U44ZpgZ5zAkF5rceo=";
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
