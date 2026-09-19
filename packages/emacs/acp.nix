{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "acp";
  version = "0.15.1-unstable-2026-09-14";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "acp.el";
    rev = "242cef63d76cc1073485847f67a21f6d8406d158";
    sha256 = "sha256-Gz8u3p+eaO2d/lD3u5i85ucNijwSATyfdndY+iJxKgg=";
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
