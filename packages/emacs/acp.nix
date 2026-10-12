{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "acp";
  version = "0.17.1-unstable-2026-10-10";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "acp.el";
    rev = "5b3abb2956d16f39ed76bc53fe31d3c2ee7fecfd";
    sha256 = "sha256-vRvUAP3Al7itI6iaqUaRXNifYp4vfGvbSATfUYCrHHg=";
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
