{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "acp";
  version = "0.20.3-unstable-2026-11-21";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "acp.el";
    rev = "041470ecff003630e03f061584d41071c36f0124";
    sha256 = "sha256-M+Y3tkHHVoT993uZcgaKogLBZOwmjIzSauylVedxfKA=";
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
