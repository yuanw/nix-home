{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "acp";
  version = "0.15.1-unstable-2026-09-08";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "acp.el";
    rev = "0f2cac4f9ee7998145a1d4710eb1ea5b2fa9d98f";
    sha256 = "sha256-qB+phi7Frs3pHptl1xY5XzBPBIf4ukFSvAzB3uAFAyQ=";
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
