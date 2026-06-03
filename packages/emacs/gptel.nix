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
  version = "0-unstable-2026-05-30";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "f342b3010f135ca4a38b02564b50b2d3e66554cf";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-qdJWgKaLZxA0BEcIOOWocrdZDh6wPXkOcF1R/szH87E=";
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
