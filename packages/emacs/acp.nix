{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "acp";
  version = "0.20.2-unstable-2026-11-19";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "acp.el";
    rev = "3fa3b58ecef062293095b046d4873b5bf4c99de3";
    sha256 = "sha256-z9HkkMvxmIEqWcvjVQX4bqmqVTBSkU9aRgtTXrCKeXc=";
  };

  packageRequires = [ ];

  recipe = writeText "recipe" ''
    (acp
     :repo "xenodium/acp.el"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "An ACP (Agent Client Protocol) implementation in Emacs lisp";
    homepage = "https://github.com/xenodium/acp.el";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
