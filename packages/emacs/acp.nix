{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "acp";
  version = "0.3.5-unstable-2026-03-20";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "acp.el";
    rev = "863f2d62c4b4da8b229581be42d490a7403b2eb1";
    sha256 = "sha256-JblbafhQlCCyFq82wXL09P2gRldrSxXdlTkIeytGISE=";
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
