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
  version = "0.9.9.4-unstable-2026-03-19";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "bbfbd711fae64b079f7057d71772805edeb00a3d";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-pArCirduxy4tGQVt41haXf7+dHzDfNrxBoAFHlFGErg=";
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
