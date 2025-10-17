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
  version = "0.9.9-unstable-2025-10-16";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "730dcc46206d7b958c2d33dc80b04fe9f17fce8d";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-dlVJBjn6IZ2agmSG8kPQRsIn0GAzzxkmw/7V7AsX0dk=";
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
