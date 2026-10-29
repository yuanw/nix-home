{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "acp";
  version = "0.18.1-unstable-2026-10-21";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "acp.el";
    rev = "2959da5fa8657da17a78ac6ceef303a50e3e69de";
    sha256 = "sha256-BF7benIKiMWQmtkJy9M4OZeLFlEZFEYXBgvzXWEf9B4=";
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
